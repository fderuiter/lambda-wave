# Lambda-Wave Documentation Index

**Last Updated:** January 28, 2026

Welcome to the Lambda-Wave documentation! This index provides a roadmap to all project documentation organized by audience and purpose.

---

## 📚 Quick Navigation by Role

### 👤 New Users / Evaluators
Start here to understand what Lambda-Wave is and get it running quickly:

1. **[README.md](../README.md)** - Project overview and quick links
2. **[QUICKSTART.md](QUICKSTART.md)** - 15-minute setup guide
3. **[PURPOSE_AND_ARCHITECTURE.md](PURPOSE_AND_ARCHITECTURE.md)** - Why this project exists and how it works

### 👨‍💻 Developers
Essential guides for contributing to the codebase:

1. **[DEVELOPER_GUIDE.md](DEVELOPER_GUIDE.md)** - Code structure and development workflow
2. **[BUILD_GUIDE.md](BUILD_GUIDE.md)** - Comprehensive build instructions
3. **[TODO.md](../TODO.md)** - Prioritized development tasks
4. **[CONTRIBUTING.md](../CONTRIBUTING.md)** - Contribution guidelines and PR process
5. **[mathematical_framework.md](mathematical_framework.md)** - Signal processing algorithms

### 🏥 Medical Physicists / Clinical Users
Clinical context and validation:

1. **[PURPOSE_AND_ARCHITECTURE.md](PURPOSE_AND_ARCHITECTURE.md)** - Medical context and SGRT overview
2. **[mathematical_framework.md](mathematical_framework.md)** - FMCW radar theory and algorithms
3. **[PROJECT_STATUS.md](PROJECT_STATUS.md)** - Current implementation status

### 🛡️ Compliance / QA Officers
Safety and regulatory documentation:

1. **[PURPOSE_AND_ARCHITECTURE.md](PURPOSE_AND_ARCHITECTURE.md)** - Safety & Compliance section (IEC 62304)
2. **[PROJECT_STATUS.md](PROJECT_STATUS.md)** - Compliance metrics and status
3. **[docs/iec_62304/](iec_62304/)** - IEC 62304 compliance artifacts
4. **[TODO.md](../TODO.md)** - Critical safety items (P0 priority)

### 📊 Project Managers / Leadership
High-level status and planning:

1. **[PROJECT_STATUS.md](PROJECT_STATUS.md)** - Comprehensive project status
2. **[TODO.md](../TODO.md)** - Development roadmap with priorities
3. **[DOCUMENTATION_REPORT.md](DOCUMENTATION_REPORT.md)** - Documentation overview

---

## 📖 Documentation by Type

### Core Documentation

| Document | Purpose | Audience | Status |
|----------|---------|----------|--------|
| [README.md](../README.md) | Project overview, quick start, badges | All | ✅ Current |
| [TODO.md](../TODO.md) | Prioritized development tasks | Developers, PM | ✅ Current |
| [CONTRIBUTING.md](../CONTRIBUTING.md) | Contribution guidelines | Developers | ✅ Current |
| [LICENSE](../LICENSE) | BSD-3-Clause license | All | ✅ Current |

### Technical Documentation

| Document | Purpose | Audience | Status |
|----------|---------|----------|--------|
| [PURPOSE_AND_ARCHITECTURE.md](PURPOSE_AND_ARCHITECTURE.md) | System design, medical context, architecture | All | ✅ Current |
| [BUILD_GUIDE.md](BUILD_GUIDE.md) | Build system, dependencies, CI/CD | Developers | ✅ Current |
| [DEVELOPER_GUIDE.md](DEVELOPER_GUIDE.md) | Code structure, workflow | Developers | ⚠️ Needs update |
| [mathematical_framework.md](mathematical_framework.md) | FMCW algorithms, theory | Developers, Physicists | ✅ Current |
| [QUICKSTART.md](QUICKSTART.md) | Installation and setup | New users | ✅ Current |

### Project Management

| Document | Purpose | Audience | Status |
|----------|---------|----------|--------|
| [PROJECT_STATUS.md](PROJECT_STATUS.md) | Implementation status, metrics | PM, Leadership | ✅ Current |
| [TODO.md](../TODO.md) | Task list with priorities | Developers, PM | ✅ Current |
| [DOCUMENTATION_REPORT.md](DOCUMENTATION_REPORT.md) | Documentation audit | Documentation team | ✅ Current |

### Legacy / Archived

| Document | Purpose | Status | Notes |
|----------|---------|--------|-------|
| [roadmap.md](../roadmap.md) | Original development plan | 🗄️ Superseded | See TODO.md instead |
| [architecture.md](architecture.md) | Early architecture notes | 🗄️ Superseded | Merged into PURPOSE_AND_ARCHITECTURE.md |
| [Haskell Radar SGRT System Development.md](../Haskell%20Radar%20SGRT%20System%20Development.md) | Detailed technical architecture | 🗄️ Archive | Historical reference |

### Templates

| Document | Purpose | Audience |
|----------|---------|----------|
| [PULL_REQUEST_TEMPLATE.md](../PULL_REQUEST_TEMPLATE.md) | PR template | Developers |
| [.github/ISSUE_TEMPLATE/bug_report.md](../.github/ISSUE_TEMPLATE/bug_report.md) | Bug report template | All |

---

## 🗺️ Documentation Flow

### For New Contributors

```
1. README.md (Overview)
   ↓
2. QUICKSTART.md (Get it running)
   ↓
3. DEVELOPER_GUIDE.md (Understand structure)
   ↓
4. BUILD_GUIDE.md (Build from source)
   ↓
5. TODO.md (Pick a task)
   ↓
6. CONTRIBUTING.md (Submit PR)
```

### For Understanding the System

```
1. README.md (What is it?)
   ↓
2. PURPOSE_AND_ARCHITECTURE.md (Why and how?)
   ↓
3. mathematical_framework.md (Technical details)
   ↓
4. PROJECT_STATUS.md (Current state)
```

### For Clinical Validation

```
1. PURPOSE_AND_ARCHITECTURE.md (Medical context)
   ↓
2. mathematical_framework.md (Algorithm validation)
   ↓
3. PROJECT_STATUS.md (Validation status)
   ↓
4. docs/iec_62304/ (Compliance docs)
```

---

## 📂 Directory Structure

```
lambda-wave/
├── README.md                    # Project overview
├── TODO.md                      # Development tasks (NEW)
├── CONTRIBUTING.md              # How to contribute
├── LICENSE                      # BSD-3-Clause
├── roadmap.md                   # (SUPERSEDED by TODO.md)
│
├── docs/                        # All documentation
│   ├── README.md               # This index file
│   ├── PURPOSE_AND_ARCHITECTURE.md  # Core technical doc
│   ├── BUILD_GUIDE.md          # Build instructions
│   ├── DEVELOPER_GUIDE.md      # Developer workflow
│   ├── PROJECT_STATUS.md       # Current status
│   ├── QUICKSTART.md           # Quick setup
│   ├── DOCUMENTATION_REPORT.md # Doc audit
│   ├── mathematical_framework.md # Algorithms
│   ├── architecture.md         # (SUPERSEDED)
│   └── iec_62304/              # Compliance artifacts
│
├── .github/
│   ├── ISSUE_TEMPLATE/
│   │   └── bug_report.md       # Issue template
│   └── workflows/              # CI/CD
│
└── Haskell Radar SGRT System Development.md  # (ARCHIVED)
```

---

## 🔄 Documentation Maintenance

### Update Frequency

- **README.md**: As needed when project changes
- **TODO.md**: Weekly during sprint planning
- **PROJECT_STATUS.md**: Monthly or after major milestones
- **BUILD_GUIDE.md**: When build process changes
- **DEVELOPER_GUIDE.md**: When code structure changes
- **PURPOSE_AND_ARCHITECTURE.md**: Rarely (stable architecture)

### Review Process

1. **Weekly**: Review TODO.md during sprint planning
2. **Monthly**: Review PROJECT_STATUS.md for accuracy
3. **Quarterly**: Full documentation audit
4. **Release**: Update all version-specific information

### Ownership

| Document Type | Owner |
|---------------|-------|
| Technical docs | Lead Developer |
| Process docs | Project Manager |
| Compliance docs | QA/Safety Officer |
| User docs | Technical Writer / Developer |

---

## 🆘 Common Questions

### "Where do I start?"
→ [README.md](../README.md) for overview, then [QUICKSTART.md](QUICKSTART.md)

### "How do I build the project?"
→ [BUILD_GUIDE.md](BUILD_GUIDE.md)

### "What tasks need to be done?"
→ [TODO.md](../TODO.md)

### "How does the system work technically?"
→ [PURPOSE_AND_ARCHITECTURE.md](PURPOSE_AND_ARCHITECTURE.md)

### "What's the current project status?"
→ [PROJECT_STATUS.md](PROJECT_STATUS.md)

### "How do I contribute?"
→ [CONTRIBUTING.md](../CONTRIBUTING.md)

### "What are the medical/clinical details?"
→ [PURPOSE_AND_ARCHITECTURE.md](PURPOSE_AND_ARCHITECTURE.md) sections 1-2

### "What are the algorithms?"
→ [mathematical_framework.md](mathematical_framework.md)

### "Is this compliant with medical device regulations?"
→ [PURPOSE_AND_ARCHITECTURE.md](PURPOSE_AND_ARCHITECTURE.md) safety section, [docs/iec_62304/](iec_62304/)

---

## 📝 Contributing to Documentation

Found an error or want to improve documentation?

1. Small fixes: Edit directly and submit PR
2. Large changes: Open issue first to discuss
3. New documents: Follow template in CONTRIBUTING.md
4. Always update this index when adding/removing documents

---

## 📞 Documentation Feedback

Have suggestions for improving documentation?

- **Issues**: [GitHub Issues](https://github.com/fderuiter/lambda-wave/issues)
- **Discussions**: [GitHub Discussions](https://github.com/fderuiter/lambda-wave/discussions)
- **Maintainer:** Frederick de Ruiter ([@fderuiter](https://github.com/fderuiter))
- **Email**: fpderuiter@gmail.com

---

**Index Maintained by:** Documentation Team  
**Last Full Audit:** January 28, 2026  
**Next Review:** February 2026
