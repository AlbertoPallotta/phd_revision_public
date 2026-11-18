"""Delivery support. Archive membership and content verification.

Arguments: DELIVERY SOURCE_INVENTORY PROJECTION NEW_LOG INVENTORY_SHA256 PROJECTION_SHA256.
Both CSV controls are derived from the agreed specification, not the current tree.
No extraction, R invocation, build, or model execution occurs in this tool.
"""
import csv, hashlib, json, re, stat, sys, tarfile, zipfile
from pathlib import Path


def digest(data):
    return hashlib.sha256(data).hexdigest()


def require(condition, message):
    if not condition:
        raise ValueError(message)


def load_control(path, expected):
    require(re.fullmatch(r"[0-9a-fA-F]{64}", expected), "Unassigned control identity")
    require(digest(path.read_bytes()) == expected.lower(), "Control identity mismatch: " + str(path))
    with path.open(encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream))


def normalized(name, directory=False):
    name = name.replace("\\", "/")
    require(not name.startswith("/") and ":" not in name, "Rooted/stream archive path: " + name)
    if directory:
        name = name.rstrip("/")
    parts = name.split("/")
    require(all(p not in ("", ".", "..") and not p.endswith((" ", ".")) for p in parts),
            "Noncanonical member: " + name)
    return "/".join(parts).casefold()


def dcf(data):
    # R build refolds fields and may change line endings; field values are compared.
    fields = {}; key = None
    for line in data.decode("utf-8-sig").splitlines():
        if not line.strip():
            continue
        if line[0].isspace():
            require(key is not None, "Orphan DESCRIPTION continuation")
            fields[key] += " " + line.strip()
        else:
            match = re.fullmatch(r"([^:\s]+):\s*(.*)", line)
            require(match is not None, "Invalid DESCRIPTION line")
            key = match[1]
            require(key not in fields, "Duplicate DESCRIPTION field: " + key)
            fields[key] = match[2]
    return {k: re.sub(r"\s+", " ", v).strip() for k, v in fields.items()}


def check_description(source, built):
    before, after = dcf(source), dcf(built)
    added = {"NeedsCompilation", "Packaged", "Author", "Maintainer"}
    require(not (added & set(before)), "Unexpected existing generated DESCRIPTION field; review projection")
    require(set(after) == set(before) | added, "Unexpected/missing R-build DESCRIPTION fields")
    for key, value in before.items():
        require(after[key] == value, "R build changed source DESCRIPTION field: " + key)
    require(after["NeedsCompilation"] == "no", "Unexpected compilation metadata")
    require(after["Author"] == "Alberto Pallotta [aut, cre]", "Unexpected derived Author")
    require(after["Maintainer"] == "Alberto Pallotta <pallottaalberto@gmail.com>", "Unexpected Maintainer")
    require(re.fullmatch(r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} UTC; alber", after["Packaged"]),
            "Unexpected Packaged format/user; do not widen the projection silently")
    return {key: after[key] for key in sorted(added)}


def archive_files(archive, kind):
    files = {}; seen = set(); spellings = {}
    if kind == "zip":
        with zipfile.ZipFile(archive) as stream:
            for member in stream.infolist():
                is_dir = member.is_dir(); key = normalized(member.filename, is_dir)
                require(key not in seen, "Duplicate normalized ZIP member: " + member.filename)
                seen.add(key)
                require(stat.S_IFMT(member.external_attr >> 16) != stat.S_IFLNK, "ZIP symlink prohibited")
                if not is_dir:
                    files[key] = stream.read(member); spellings[key] = member.filename
    else:
        with tarfile.open(archive, "r:gz") as stream:
            for member in stream.getmembers():
                key = normalized(member.name, member.isdir())
                require(key not in seen, "Duplicate normalized tar member: " + member.name)
                seen.add(key)
                require(member.isdir() or member.isfile(), "Nonregular tar member prohibited: " + member.name)
                if member.isfile():
                    files[key] = stream.extractfile(member).read(); spellings[key] = member.name
    return files, spellings


def main(arguments):
    require(len(arguments) == 6, "Expected six arguments")
    delivery, inventory_path, projection_path, log = map(Path, arguments[:4])
    delivery = delivery.resolve(); log = log.resolve()
    require(not log.is_relative_to(delivery), "Check log must be outside delivery")
    require(not log.exists() and log.parent.is_dir(), "Use a new file in an existing external log directory")
    report = {"status": "FAIL", "scope": "Selected source tree, exact source ZIPs and explicit R-build projection",
              "inventory_sha256": arguments[4], "projection_sha256": arguments[5], "packages": {}, "error": ""}
    try:
        inventory = load_control(inventory_path, arguments[4])
        projection = load_control(projection_path, arguments[5])
        require({r["package"] for r in inventory} == {"TradeTesterR", "PortfolioTesteR"}, "Unexpected package set")
        require({r["package"] for r in projection} == {"TradeTesterR", "PortfolioTesteR"}, "Unexpected projection package")
        for package in ("TradeTesterR", "PortfolioTesteR"):
            expected = {}; plan = {}
            for row in (r for r in inventory if r["package"] == package):
                key = normalized(row["source_relative_path"])
                require(key not in expected, "Duplicate source inventory path")
                require(re.fullmatch(r"[0-9a-f]{64}", row["sha256"]), "Source inventory identity pending")
                expected[key] = row
            for row in (r for r in projection if r["package"] == package):
                key = normalized(row["source_relative_path"])
                require(key not in plan, "Duplicate projection path")
                require(row["tar_rule"] in ("COPY", "EXCLUDE", "DESCRIPTION_FIELDS"), "Unapproved projection rule")
                plan[key] = row
            require(set(plan) == set(expected), "Projection does not cover exactly the selected source inventory")
            source_root = delivery / "packages" / package
            actual = {}
            for file in source_root.rglob("*"):
                require(not file.is_symlink() and not file.is_junction(), "Source links/junctions prohibited")
                if file.is_file():
                    key = normalized(file.relative_to(source_root).as_posix())
                    require(key not in actual, "Duplicate normalized source path")
                    actual[key] = file.read_bytes()
            evidence = {"source_status": "FAIL", "zip_status": "NOT RUN", "tar_status": "NOT RUN",
                        "source_missing": sorted(set(expected) - set(actual)),
                        "source_extra": sorted(set(actual) - set(expected)), "archives": {}}
            report["packages"][package] = evidence
            require(set(actual) == set(expected), "Source tree membership differs: " + package)
            for key, row in expected.items():
                require(digest(actual[key]) == row["sha256"] and len(actual[key]) == int(row["bytes"]),
                        "Selected source content differs: " + package + "/" + key)
            evidence["source_status"] = "PASS"
            version = dcf(actual["description"])["Version"]
            for kind, suffix in (("zip", "_examination_source.zip"), ("tar", ".tar.gz")):
                archive = delivery / "archives" / (package + "_" + version + suffix)
                members, spellings = archive_files(archive, kind)
                wanted = {}
                for key, row in plan.items():
                    if kind == "tar" and row["tar_rule"] == "EXCLUDE":
                        require(row["tar_member"] == "" and row["permitted_reason"], "Exclusion must be named and explained")
                        continue
                    member = row["zip_member"] if kind == "zip" else row["tar_member"]
                    mk = normalized(member)
                    require(mk == normalized(package + "/" + row["source_relative_path"]), "Unexpected member relocation")
                    require(mk not in wanted, "Duplicate expected member")
                    wanted[mk] = key
                entry = {"path": str(archive), "sha256": digest(archive.read_bytes()), "status": "FAIL",
                         "missing": sorted(set(wanted) - set(members)), "extra": sorted(set(members) - set(wanted)), "members": []}
                evidence["archives"][kind] = entry
                require(set(members) == set(wanted), "Archive membership differs: " + str(archive))
                for member, key in wanted.items():
                    rule = plan[key]["tar_rule"] if kind == "tar" else "COPY"
                    metadata = {}
                    if rule == "DESCRIPTION_FIELDS":
                        require(key == "description", "Only DESCRIPTION may use the field rule")
                        metadata = check_description(actual[key], members[member])
                    else:
                        require(members[member] == actual[key], "Archive content differs: " + member)
                    entry["members"].append({"member": spellings[member], "normalized": member,
                                              "sha256": digest(members[member]), "bytes": len(members[member]),
                                              "rule": rule, "metadata": metadata, "status": "PASS"})
                entry["status"] = "PASS"; evidence[kind + "_status"] = "PASS"
        report["status"] = "PASS"
    except Exception as error:
        report["error"] = str(error)
    log.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    return 0 if report["status"] == "PASS" else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
