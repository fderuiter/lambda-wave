# Forge's Journal

## 2024-05-22 - [Centralized Dependency Management] **Learning:** Hardcoded `apt-get` commands in Dockerfiles and CI workflows create fragility and make local setup difficult for non-Linux users. **Action:** Extracted dependency installation into a polyglot shell script (`scripts/setup_env.sh`) that supports both apt and brew, and referenced this script in Dockerfile and CI to ensure a Single Source of Truth.
