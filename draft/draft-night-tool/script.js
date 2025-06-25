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
    } else if (id == "#mark-board") {
        my_col = "grey";
    } else {
        my_col = "#31302F";
    }

    let tbody = table.append("tbody");
    tbody.selectAll("tr")
        .data(top5)
        .enter()
        .append("tr")
        .selectAll("td")
        .data((d) => Object.entries(d).map(([key, value]) => ({ key, value })))
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
        .derive({trade_seq: aq.escape((d) => {
                return d.trade_seq.map(team => {
                    return t_data.filter(t => t.team_abbreviation == team)[0].team_logo;
                })
            })
        })
        .derive({
            team_seq_logo: (d) => [d.team_logo, ...d.trade_seq]
        })
        .select({ pick: 'Pick No.', team_seq_logo: 'Team_Seq_Logo', selection: 'Selection' })
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
        .data((d) => Object.entries(d).map(([key, value]) => ({ key, value })))
        .enter()
        .append("td")
        .attr("class", (d) => "position-relative " + (d.key == "Team" ? "text-center" : ""))
        .style("width", (d) => d.key == "Pick No." ? "10%" : (d.key == "Team_Seq_Logo" ? "15%" : "75%"))
        .html((d) => {
            if(d.key == "Team_Seq_Logo") {
                const logos = [...d.value].map((logo, i) => {
                    const teams_count = d.value.length - 1;
                    let size = "40px";
                    let grayscale = "";
                    let y_adj = "";
                    if(teams_count > 0) {
                        size = (i == teams_count) ? "32px" : "15px";
                        grayscale = (i == teams_count) ? "position-absolute" : "grayscale";
                        y_adj = (i == teams_count) ? "top:18px;left:8px" : "";
                    }
                    return `<img class="${grayscale}" style="${y_adj};width:${size};height:${size};" src="${logo}">`;
                });
                return `
                ${logos[logos.length - 1]}
                <div class="position-absolute d-flex" style="top: 3px;">
                ${logos.filter((d,n) => n != (logos.length - 1)).join("\n")}
                </div>
                `;
            } else {
                if(d.key == "Pick No.") {
                    return `<div class="h4 pick-cell text-center">${d.value}</div>`
                } else {
                    return `<div class="h4 selection-cell text-uppercase">${d.value}</div>`
                }

            }
         });
}

function show_prospect(results, prospects, id) {
    let latest_pick = aq.from(results)
        .filter(d => d.selection != 0)
        .slice(-1)
        .objects()[0];

    if(!latest_pick) {
        return; // Abort any rendering
    }

    let prosp_data = aq.from(prospects)
        .filter(aq.escape(d => d.abbr_name == latest_pick.selection))
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
            name: "",
            abbr_name: latest_pick.selection,
            weight: "-"
        }
    } else {
        prosp_data = prosp_data[0]
    }

    d3.select(id)
        .append("div")
        .attr("class", "row justify-content-center mt-3")
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
        .filter(p => ['age','height','length','bmi','weight'].includes(p[0]))
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
        .attr("class", "row row-cols-5 g-2 my-3")
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

    // Distribution of mocks
    const mocks = aq.from(prosp_data.mocks)
        .groupby("rank")
        .derive({num: (d) => aq.op.row_number()})
        .objects();

    const ydomain = d3.extent(d3.map(mocks, d => d.num));
    const yrange = [0, d3.max([1,ydomain[1]]) + 1];
    const xdomain = d3.extent(d3.map(mocks, d => d.rank));
    const xrange = [d3.max([1, xdomain[0] - 1]), d3.max([4, xdomain[1] + 1])];

    const tickCount = d3.min([10, d3.count([... new Set(d3.map(mocks, d => d.rank))]) + 2]);
    const img_size = d3.min([
        (400 / yrange[1]) - 20, // Height consideration
        (1000 / (xrange[1] - xrange[0])) - 20 // Width consideration
    ]);

    const plot = Plot.plot({
        width: 1000,
        height: 400,
        style: {background: "#ffffff"},
        marginLeft: 40, marginRight: 40,
        x: {
          domain: xrange,
          tickFormat: (d) => d3.format("d")(d),
          ticks: tickCount,
          tickSize: (d) => 0,
          label: null
        },
        y: {domain: yrange, tickFormat: (d) => null, tickSize: (d) => 0, label: null},
        marks: [
          Plot.image(mocks, {
            x: "rank",
            y: "num",
            src: "logo",
            width: img_size,
            title: "source"
          })
        ]
      });

      document.getElementById(id.replace("#","")).appendChild(plot);
}

function show_over_under(results, over_under) {
    const ou_wrapper = d3.select("#overunder");

    const card = ou_wrapper.selectAll()
        .data(over_under)
        .enter()
        .append('div').attr("class", "col")
        .append('div').attr("class", "card h-100")
        .append('div').attr("class", "card-body");

    // Add title
    card.append("div")
        .attr("class", d => {
            // Custom styling for the actual value vs predicted
            return `$card-title text-center position-relative`;
        })
        .html(d => {
            // Custom styling for the actual value vs predicted
            const actual = d.actual_value != "" ? `<h4 class="position-absolute top-0 end-0 text-bg-warning px-3 rounded-4">${d.actual_value}</h4>` : "";
            return `
                <h4 class="display-4">${d.ou_value}</h4>
                ${actual}
            `;
        });
    // Add title
    card.append("p").attr("class", "text-center").text(d => d.description);
    // Add horizonal rule
    card.append("hr");
    // Add the bets
    const bets_wrapper = card.append("div").attr("class", "d-flex justify-content-evenly");
    // Unders
    const bets_under = bets_wrapper.append("div").attr("class", "pick-under");
    bets_under.append("p").attr("class", "text-center").html(d => {
        const truly_under = (d.actual_value != "") & (d.confirmed) & (d.actual_value < d.ou_value)
        const truly_under_classes = truly_under ? "text-bg-warning px-2 py-1 rounded" : "";
        return `<span class="${truly_under_classes}">Under</span>`;
    });
    bets_under.selectAll("img")
        .data(d => d.under)
        .enter()
        .append("img")
        .attr("class", "ou-img rounded-circle")
        .attr("src", d => d.toLowerCase() + '-image.png');
    // Vertical separator
    const vr = bets_wrapper.append("div").attr("class", "vr");
    // Overs
    const bets_over = bets_wrapper.append("div").attr("class", "pick-over");
    bets_over.append("p").attr("class", "text-center").html(d => {
        const truly_over = (d.actual_value != "") & (d.confirmed) & (d.actual_value > d.ou_value) ? "text-bg-warning px-2 py-1 rounded" : "";
        const truly_over_classes = truly_over ? "text-bg-warning px-2 py-1 rounded" : "";
        return `<span class="${truly_over_classes}">Over</span>`;
    });
    bets_over.selectAll("img")
        .data(d => d.over)
        .enter()
        .append("img")
        .attr("class", "ou-img rounded-circle")
        .attr("src", d => d.toLowerCase() + '-image.png');
}

function resume_scroll(results) {
    const count = results.filter(r => ![undefined, ''].includes(r.selection)).length;
    const board = document.getElementById("master-board");

    const height = board.scrollHeight - board.clientHeight;
    const pos = height * (count > 17 ? 1 : count < 13 ? 0 : 0.5);

    board.scrollTop = pos;
}

function render() {
    Promise.all([
        d3.csv('adam_board.csv'),
        d3.csv('josh_board.csv'),
        d3.csv('mark_board.csv'),
        d3.csv('industry_board.csv'),
        d3.json('results.json'),
        d3.csv('team_list.csv'),
        d3.json('prospects.json'),
        d3.json('over-under.json')
    ]).then(([ab_data, jb_data, mb_data, ind_data, results, teams, prospects, over_under]) => {
        unavailable = results
            .map(d => d.selection)
            .filter(x => x && x !== 'Forfeited');

        show_top_5(ab_data, "#adam-board");
        show_top_5(jb_data, "#josh-board");
        show_top_5(mb_data, "#mark-board");
        show_top_5(ind_data, "#industry-board");
        show_full_board(results, teams, "#master-board");
        show_prospect(results, prospects, "#prospect-data");
        show_over_under(results, over_under);
        resume_scroll(results);
    })
};

render();