document.addEventListener("DOMContentLoaded", function() {
    // Wait for MkDocs to render the navigation
    setTimeout(initToggle, 100);
});

function initToggle() {
    const navItems = document.querySelectorAll('.md-nav--primary > ul.md-nav__list > li.md-nav__item--nested');
    if (navItems.length === 0) return;

    let roleBasedItem = null;
    let typeBasedItem = null;

    navItems.forEach(item => {
        const label = item.querySelector('label');
        if (label) {
            const text = label.textContent.trim();
            if (text.includes("Role-based")) {
                roleBasedItem = item;
            } else if (text.includes("Type-based")) {
                typeBasedItem = item;
            }
        }
    });

    if (!roleBasedItem || !typeBasedItem) return;

    // Create Toggle Container
    const toggleContainer = document.createElement('div');
    toggleContainer.className = 'nav-toggle-container';
    
    const toggleBtn = document.createElement('button');
    toggleBtn.type = 'button';
    toggleBtn.className = 'nav-toggle-btn';
    
    const primaryNav = document.querySelector('.md-nav--primary');
    if (primaryNav) {
        if (!primaryNav.id) {
            primaryNav.id = 'primary-nav-container';
        }
        toggleBtn.setAttribute('aria-controls', primaryNav.id);
    }
    
    // Initial State
    let currentView = localStorage.getItem('navView') || 'Role-based';
    
    function applyView(restoreFocus) {
        if (currentView === 'Role-based') {
            roleBasedItem.classList.remove('md-nav__item--hidden');
            typeBasedItem.classList.add('md-nav__item--hidden');
            toggleBtn.textContent = 'Switch to Type-based (Diátaxis)';
            toggleBtn.setAttribute('aria-pressed', 'false');
        } else {
            roleBasedItem.classList.add('md-nav__item--hidden');
            typeBasedItem.classList.remove('md-nav__item--hidden');
            toggleBtn.textContent = 'Switch to Role-based';
            toggleBtn.setAttribute('aria-pressed', 'true');
        }
        localStorage.setItem('navView', currentView);
        if (restoreFocus) {
            toggleBtn.focus();
        }
    }

    toggleBtn.addEventListener('click', function() {
        currentView = currentView === 'Role-based' ? 'Type-based' : 'Role-based';
        applyView(true);
    });

    applyView(false);
    toggleContainer.appendChild(toggleBtn);
    
    // Insert toggle button at the top of the sidebar
    const sidebar = document.querySelector('.md-sidebar--primary .md-sidebar__scrollwrap');
    if (sidebar) {
        sidebar.insertBefore(toggleContainer, sidebar.firstChild);
    }
}
