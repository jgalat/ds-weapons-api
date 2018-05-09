import urllib.request
import json
import re
from bs4 import BeautifulSoup
from sys import argv

url_base = "http://darksoulsdatabase.com"
weapons = {}

def read_val(value):
    if value == "–":
        return None
    elif '%' in value:
        return read_val(value.replace('%', ''))
    elif '.' in value:
        return float(value)
    elif value.isnumeric():
        return int(value)
    else:
        return value

def get_text(soup_list, separator):
    return separator.join(map(lambda x: x.get_text(), soup_list))

def scrap_weapon_types(href):
    url = url_base + href
    page = urllib.request.urlopen(url)
    soup = BeautifulSoup(page, 'lxml')
    return soup.select('div.subheadlinks a')

def scrap_weapon(href):
    weapon_name = href.split('/')[-1]

    url_weapon = url_base + href
    print("\tScraping weapon: " + weapon_name)

    weapon_page = urllib.request.urlopen(url_weapon)
    weapon_soup = BeautifulSoup(weapon_page, 'lxml')

    weapon = {}

    # Name, Description, Special Features
    weapon['name'] = weapon_soup.find('h1', class_='detail_title').contents[0]
    weapon['description'] = get_text(weapon_soup.select('div.description p'), '\n')

    weapon['special_features'] = None
    special_feature = weapon_soup.select('h3.special_modifier')
    if len(special_feature) > 0:
        weapon['special_features'] = get_text(special_feature[0].find_next_siblings('p'), '\n')
    weapon['weapon_type'] = weapon_soup.select('div.detail_picture span')[0].get_text()

    # Weight, Stability, Durability, Critical
    fourth_containers = weapon_soup.select('div.fourth.container')
    for container in fourth_containers:
        stat = container.h3.contents[0]
        weapon[stat] = read_val(container.find('span', class_='num').contents[0])

    # Damage, Damage Reduction, Requirements, Auxiliary, Bonus
    third_containers = weapon_soup.select('div.third.container')
    damage_type_container = third_containers.pop()
    for container in third_containers:
        stat_type = container.h3.contents[0].replace(' ', '_')
        if stat_type == 'reduction':
            stat_type = 'damage_reduction'
        stats = container.find_all('span', class_='')
        weapon[stat_type] = {}
        for stat in stats:
            stat_name = stat.contents[0]
            if stat_name == 'normal':
                stat_name = 'physical'
            sibling = stat.find_previous_sibling('span', class_='num')
            weapon[stat_type][stat_name] = read_val(sibling.contents[0])

    # Attack Type
    attack_types = damage_type_container.find_all('span', class_='num')
    attack_types_ok = filter(lambda x: int(re.findall('\d+', x.attrs['style'])[0]) > 1, attack_types)
    attack_types_desc = map(lambda x: x.find_next_sibling('span', class_=''), attack_types_ok)
    attack_type = get_text(attack_types_desc, '/')
    weapon['attack_type'] = attack_type

    # Upgrades
    upgrades = {}
    upgrades_containers = weapon_soup.select('div.weapon_details_upgrade_container div.detailstable')
    for upgrade_container in upgrades_containers:
        upgrade_name = upgrade_container.h4.span.attrs['id']
        rows = upgrade_container.table.select('tr')
        upgrade = {}
        for row in rows:
            data = row.contents
            plus_val = data[0].contents[0]
            upgrade[plus_val] = {}
            damage = {}
            damage_reduction = {}
            bonus = {}

            damage['physical'] = read_val(data[1].contents[0])
            damage['magic'] = read_val(data[2].contents[0])
            damage['fire'] = read_val(data[3].contents[0])
            damage['lightning'] = read_val(data[4].contents[0])

            damage_reduction['physical'] = read_val(data[5].contents[0])
            damage_reduction['magic'] = read_val(data[6].contents[0])
            damage_reduction['fire'] = read_val(data[7].contents[0])
            damage_reduction['lightning'] = read_val(data[8].contents[0])

            bonus['strength'] = read_val(data[9].contents[0])
            bonus['dexterity'] = read_val(data[10].contents[0])
            bonus['intelligence'] = read_val(data[11].contents[0])
            bonus['faith'] = read_val(data[12].contents[0])

            upgrade[plus_val]['damage'] = damage
            upgrade[plus_val]['damage_reduction'] = damage_reduction
            upgrade[plus_val]['bonus'] = bonus

        upgrades[upgrade_name] = upgrade

    weapon['upgrades'] = upgrades
    weapons[weapon_name] = weapon

def scrap_weapon_type(href):
    url = url_base + href
    weapon_type = href.split('/')[-1].split('?')[0]
    print("Scraping weapons with type: " + weapon_type)
    weapon_type_page = urllib.request.urlopen(url)
    weapon_type_soup = BeautifulSoup(weapon_type_page, 'lxml')
    weapons_links = weapon_type_soup.select('table.weapontable a',)
    for weapon_link in weapons_links:
        href = weapon_link.attrs['href']
        scrap_weapon(href)

def scrap_weapons(filename):
    weapon_types = scrap_weapon_types("/compact/weapon/axe?order=asc&sort=name")
    for weapon_type in weapon_types:
        href = weapon_type.attrs['href']
        scrap_weapon_type(href)
    with open(filename, "w") as output:
        print("Writing data to: " + filename)
        output.write(json.dumps(weapons, indent = 2))

if __name__ == "__main__":
    print("Init scraping")

    if len(argv) < 2:
        print("Usage: " + argv[0] + " OUTPUT_FILE")
        exit(1)

    scrap_weapons(argv[1])

    print("Finished scraping")
    exit(0)
