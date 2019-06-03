pub use semver::*;

#[cfg(test)]
mod test {
    use super::*;
    mod version {
        use super::*;
        fn assert_version(
            version: Version,
            (major, minor, patch, prerelease, build): (u64, u64, u64, &[&str], &[&str]),
        ) {
            assert_eq!(version.major, major);
            assert_eq!(version.minor, minor);
            assert_eq!(version.patch, patch);
            assert_eq!(
                version
                    .pre
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>(),
                prerelease
            );
            assert_eq!(
                version
                    .build
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>(),
                build
            );
        }

        mod new {
            use super::*;
            #[test]
            fn text() {
                assert_version(
                    Version::parse("1.2.3-pre.4+build.5").unwrap(),
                    (1, 2, 3, &["pre", "4"], &["build", "5"]),
                );
            }
        }
    }
}
