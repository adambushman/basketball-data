let unavailable;

function show_top_5(data, id) {
    let top5 = aq.from(data)
        .filter(aq.escape( d => !unavailable.includes(d.prospect) ))
        .select({ pick_no: 'Rank', prospect: 'Prospect'})
        .slice(0, 5)
        .objects();

    let columns; 
    
    top5.forEach((d) => {
        columns = Object.keys(d)
    });

    let table = d3.select(id)
        .append("table")
        .attr("id", "picks-table");

    let my_col;
    
    if(id == "#adam-board") {
        my_col = "#C9082A";
    } else if (id == "#josh-board") {
        my_col = "#17408B";
    } else {
        my_col = "grey";
    }

    let tbody = table.append("tbody");   
    tbody.selectAll("tr")
        .data(top5)
        .enter()
        .append("tr")
        .selectAll("td")
        .data((d) => { return d3.entries(d) })
        .enter()
        .append("td")
        .style("width", (d) => d.key == "Rank" ? "15%" : "85%")
        .html((d) => { 
            if(d.key == "Rank") {
                return `<div class="h4 rank-cell text-center" style="background-color:${my_col}">${d.value}</div>`
            } else {
                return `<div class="h4 selection-cell text-uppercase text-truncate">${d.value}</div>`
            }
        });
}

function show_full_board(r_data, t_data, id) {
    let full_board = aq.from(r_data)
        .join_left(
            aq.from(t_data), 
            ['team', 'team_abbreviation']
        )
        .select({ pick: 'Pick No.', team_logo: 'Team', selection: 'Selection' })
        .orderby('Pick No.')
        .objects();

    let columns; 
    
    full_board.forEach((d) => {
        columns = Object.keys(d)
    });

    let table = d3.select(id)
        .append("table")
        .attr("class", "");

    let tbody = table.append("tbody");   
    tbody.selectAll("tr")
        .data(full_board)
        .enter()
        .append("tr")
        .selectAll("td")
        .data((d) => { return d3.entries(d) })
        .enter()
        .append("td")
        .attr("class", (d) => d.key == "Team" ? "text-center" : "")
        .style("width", (d) => d.key == "Pick No." ? "10%" : d.key == "Team" ? "15%" : "75%")
        .html((d) => { 
            if(d.key == "Team") {
                return `<img style="width:40px;height:40px;" src="${d.value}">`;
            } else {
                if(d.key == "Pick No.") {
                    return `<div class="h4 pick-cell text-center">${d.value}</div>`
                } else {
                    return `<div class="h4 selection-cell text-uppercase">${d.value}</div>`
                }
                
            }
         });
}

function abbreviate(name) {
    let split_pos = name.indexOf(" ");
    let first = name.substring(0, 1);
    let last = name.substring(split_pos + 1);

    return(`${first}. ${last}`)
}

function show_prospect(results, prospects, id) {
    let latest_pick = aq.from(results)
        .filter(d => d.selection != 0)
        .slice(-1)
        .objects()[0];

    let prosp_data = aq.from(prospects)
        .derive({name: aq.escape(d => abbreviate(d.name))})
        .filter(aq.escape(d => d.name == latest_pick.selection))
        .slice(0)
        .objects();

    if(prosp_data.length == 0) {
        prosp_data = {
            age: "-", 
            color: "-", 
            club: "-", 
            headshot: "https://media.istockphoto.com/id/1469198507/vector/default-avatar-male-profile-user-profile-icon-profile-picture-portrait-symbol-user-member.jpg?s=612x612&w=0&k=20&c=PBennsFimg3ChqUXSG2xNBPd2BuHWFk01kbHvsT9olY=", 
            height: "-", 
            wingspan: "-", 
            length: "-", 
            bmi: "-", 
            off_pos: "-", 
            def_pos: "-", 
            logo: "https://media.istockphoto.com/id/1222357475/vector/image-preview-icon-picture-placeholder-for-website-or-ui-ux-design-vector-illustration.jpg?s=612x612&w=0&k=20&c=KuCo-dRBYV7nz2gbk4J9w1WtTAgpTdznHu55W9FjimE=", 
            name: latest_pick.selection, 
            weight: "-"
        }
    } else {
        prosp_data = prosp_data[0]
    }

    console.log(prosp_data);

    d3.select(id)
        .append("div")
        .attr("class", "row justify-content-center")
        .html(`
        <div class="col-3 pic-container">
            <img src="${prosp_data.headshot}" alt="${prosp_data.headshot} Headshot" class="pic1">
            <img src="${prosp_data.logo}" alt="${prosp_data.logo} Logo" class="pic2">
        </div>
        <div class="col-5 text-white d-flex align-items-center">
            <div>
                <h1 class="prosp-name">${prosp_data.name}</h1>
                <h4 class="prosp-club fw-normal">${prosp_data.club}</h4>
            </div>
        </div>
        `);

    const attr_data = Object.entries(prosp_data)
        .filter(p => ['age','height','wingspan','length','bmi','weight'].includes(p[0]))
        .map(p => {
            key = p[0] == 'bmi' ? "BMI" : p[0][0].toUpperCase() + p[0].substring(1);
            if(["length", "wingspan"].includes(p[0])) {
                if(['', undefined].includes(p[1])) {
                    value = "?";
                } else {
                    value = (p[1] > 0 ? "+" : "") + (p[0] == "length" ? d3.format(".1f")(p[1]) : p[1]) + (p[0] == "wingspan" ? "" : "\"");
                }
            } else {
                value = p[1];
            }

            return [key, value];
        })
        .map(p => {
            key = p[0];
            value = ["Age", "BMI"].includes(p[0]) ? d3.format(".1f")(p[1]) : p[1];
            return [key, value];
        })
        .map(p => {
            return [p[0], [undefined, "NaN", "NaN\""].includes(p[1]) ? "-" : p[1]]
        });

    d3.select(id)
        .append("div")
        .attr("class", "row row-cols-3 g-2 mt-4")
        .selectAll()
        .data(attr_data)
        .enter()
        .append("div")
        .attr("class", "col")
        .html(d => `
            <div class="card">
                <div class="card-body text-center">
                    <h3 class="fw-bold m-0">${d[1]}</h3><p class="m-0"><small>${d[0]}</small></p>
                </div>
            </div>
        `);
}

function resume_scroll(results) {
    const count = results.filter(r => ![undefined, ''].includes(r.selection)).length;
    const board = document.getElementById("master-board");

    const height = board.scrollHeight - board.clientHeight;
    const pos = height * (count > 17 ? 1 : count < 13 ? 0 : 0.5);

    board.scrollTop = pos;
}

function render() {
    d3.csv('adam_board.csv', ab_data => {
        d3.csv('josh_board.csv', jb_data => {
            d3.csv('mark_board.csv', mb_data => {
                d3.json('results.json', results => {
                    d3.csv('team_list.csv', teams => {
                        d3.csv('prospects.csv', prospects => {

                            unavailable = d3.map(results, d => { return d.selection }).keys();
                            unavailable = unavailable.filter(x => !['', 'Forfeited'].includes(x));

                            show_top_5(ab_data, "#adam-board");
                            show_top_5(jb_data, "#josh-board");
                            show_top_5(mb_data, "#mark-board");
                            show_full_board(results, teams, "#master-board");
                            show_prospect(results, prospects, "#prospect-data");
                            resume_scroll(results);
                        })
                    })
                })
            })
        })
    })
}

render();