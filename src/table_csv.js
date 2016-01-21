csv = []
rows = $('#projection-data tr');
for(i =0;i < rows.length;i++) {
    cells = $(rows[i]).find('td,th');
    csv_row = [];
    for (j=0;j<cells.length;j++) {
        txt = cells[j].innerText;
        csv_row.push(txt.replace(",", "-"));
    }
    csv.push(csv_row.join(","));
}
output = csv.join("\n")

