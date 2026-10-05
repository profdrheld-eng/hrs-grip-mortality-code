"""Create a separate runnable tree. Never modify the package or read HRS inputs."""
import argparse
from pathlib import Path
import shutil
from verify_local_package import verify_package


def materialize(version, destination):
    package = Path(__file__).resolve().parent
    if version not in {'reported', 'corrected'}:
        raise ValueError('Unknown code version')
    destination = Path(destination)
    if destination.is_symlink() or destination.exists():
        raise ValueError("Destination must not already exist")
    destination = destination.resolve()
    if package == destination or package in destination.parents:
        raise ValueError("Use a destination outside this code package")
    verify_package(package)
    overlay = package / "corrected_version" / "overlay"
    if version == 'corrected' and not overlay.is_dir():
        raise ValueError("Corrected overlay is not assembled")
    shutil.copytree(package / "reported_version", destination)
    if version == "corrected":
        for source in sorted(overlay.rglob("*")):
            if source.is_file():
                target = destination / source.relative_to(overlay)
                target.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(source, target)
    return destination


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("version", choices=["reported", "corrected"])
    parser.add_argument("destination")
    args = parser.parse_args()
    materialize(args.version, args.destination)
    print("Created a separate runnable code tree. No analysis was run.")
