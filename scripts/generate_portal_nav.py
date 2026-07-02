import os
import re
import yaml

def slugify(text):
    text = text.lower().strip()
    text = re.sub(r'[^\w\s-]', '', text)
    text = re.sub(r'[\s_-]+', '-', text)
    return text

def parse_markdown(filepath, relative_path):
    with open(filepath, 'r') as f:
        content = f.read()

    entries = []
    
    # Split content by AT LEAST one header to easily find context
    lines = content.split('\n')
    current_title = os.path.basename(filepath).replace('.md', '')
    current_link = relative_path
    
    # Try to find H1 for the whole file
    for line in lines:
        if line.startswith('# '):
            current_title = line.replace('# ', '').strip()
            break

    roles = set()
    diataxis = set()

    for line in lines:
        # Check for headings to update context
        h_match = re.match(r'^(#{1,6})\s+(.*)', line)
        if h_match:
            current_title = h_match.group(2).strip()
            # GitHub slug format
            slug = slugify(current_title)
            current_link = relative_path
            # Reset tags for the new section
            roles = set()
            diataxis = set()

        # Check for tags
        role_match = re.search(r'<!--\s*TAG:roles?:(.*?)\s*-->', line)
        if role_match:
            r_list = [r.strip() for r in role_match.group(1).split(',')]
            for r in r_list:
                entries.append({
                    'nav_type': 'Role-based',
                    'category': r,
                    'title': current_title,
                    'link': current_link
                })

        diataxis_match = re.search(r'<!--\s*TAG:diataxis:(.*?)\s*-->', line)
        if diataxis_match:
            d_list = [d.strip() for d in diataxis_match.group(1).split(',')]
            for d in d_list:
                entries.append({
                    'nav_type': 'Type-based (Diátaxis)',
                    'category': d,
                    'title': current_title,
                    'link': current_link
                })
                
    # If no tags were found at all, add it to 'Uncategorized' to not lose content
    if not entries:
        entries.append({
            'nav_type': 'Role-based',
            'category': 'Other',
            'title': current_title,
            'link': relative_path
        })
        entries.append({
            'nav_type': 'Type-based (Diátaxis)',
            'category': 'Uncategorized',
            'title': current_title,
            'link': relative_path
        })
        
    return entries

def main():
    docs_dir = 'docs'
    all_entries = []

    for root, dirs, files in os.walk(docs_dir):
        for file in files:
            if file.endswith('.md'):
                filepath = os.path.join(root, file)
                rel_path = os.path.relpath(filepath, docs_dir)
                # Ensure forward slashes for URLs
                rel_path = rel_path.replace('\\', '/')
                entries = parse_markdown(filepath, rel_path)
                all_entries.extend(entries)

    # Build the nav tree
    nav_tree = {
        'Role-based': {},
        'Type-based (Diátaxis)': {}
    }

    for entry in all_entries:
        n_type = entry['nav_type']
        cat = entry['category']
        title = entry['title']
        link = entry['link']

        if n_type not in nav_tree:
            nav_tree[n_type] = {}
        if cat not in nav_tree[n_type]:
            nav_tree[n_type][cat] = []
        
        nav_tree[n_type][cat].append({title: link})

    # Format for MkDocs YAML
    mkdocs_nav = []
    
    for n_type in ['Role-based', 'Type-based (Diátaxis)']:
        type_list = []
        for cat in sorted(nav_tree[n_type].keys()):
            # Sort items by title
            items = nav_tree[n_type][cat]
            type_list.append({cat: items})
        mkdocs_nav.append({n_type: type_list})

    mkdocs_config = {
        'site_name': 'Integrated Knowledge Hub',
        'docs_dir': 'docs',
        'theme': {
            'name': 'material',
            'features': ['navigation.expand']
        },
        'extra_css': ['stylesheets/extra.css'],
        'extra_javascript': ['javascripts/toggle.js'],
        'nav': mkdocs_nav
    }

    with open('mkdocs.yml', 'w') as f:
        yaml.dump(mkdocs_config, f, sort_keys=False, allow_unicode=True)

    print("Generated mkdocs.yml successfully.")

if __name__ == '__main__':
    main()
