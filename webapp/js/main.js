
function jsonparser() {
	var tableRef = document.getElementById('resultList');
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}

	$.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=%221384142400000%22&endkey=%221384172149000%22', function(url_data) {
		$.each(url_data, function (i,element) {
			if ($.isArray(element) === true) {
				sortArrayBySymbol(element);
				$.each(element, function (i,value) {
					addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest);    
				});
			}
		});
	});
}      



function portfoliobuilder() {
	var tableRef = document.getElementById('resultList');
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}

	$.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=%221384142400000%22&endkey=%221384172149000%22', function(url_data) {
		$.each(url_data, function (i,element) {
			if ($.isArray(element) === true) {
				sortArrayBySymbol(element);
				$.each(element, function (i,value) {
					if (isStockSaved(value.value.symbol) === true) {
					addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest);    
				}
				});
			}
		});
	});
}      


function search() {
	var tableRef = document.getElementById('resultList');
	var searchterm = document.getElementById("sok").value.toLowerCase();
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	console.log("Searching for "+searchterm);
	$.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=%221384142400000%22&endkey=%221384172149000%22', function(url_data) {
		$.each(url_data, function (i,element) {
			if ($.isArray(element) === true) {
				sortArrayBySymbol(element);
				$.each(element, function (i,value) {
					if (value.value.symbol.toLowerCase().indexOf(searchterm) != -1 || value.value.name.toLowerCase().indexOf(searchterm) != -1) {
						console.log("Found Match");
					addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest);    
				} else {
				// console.log("No Match "+ value.value.symbol);
			}
				});
			}
		});
	});
}    

function getstockhistory(symbol,timeframe,name) {
	setName(name);
	duration = timeframe;
	today = Date.now();
	console.log(today.valueOf());
	if (timeframe === "week") {
		timeframe = Date.today().add(-7).days();
		timeframe = timeframe.valueOf();
		console.log(timeframe);
	}
	if (timeframe === "month") {
		timeframe = Date.today().add(-30).days();
		timeframe = timeframe.valueOf();
		console.log(timeframe);
	} if (timeframe === "today") {
		timeframe = Date.today();
		timeframe = timeframe.valueOf();
		console.log(timeframe);
	}

	var tableRef = document.getElementById('resultList');
	var date1 = null;
	var date2 = null;
	datelist = [];
	datalist = [];
	latestlist = [];
	changelist = [];
	percentlist = [];

	console.log('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse_stock?startkey=[%22'+symbol+'%22,%22'+timeframe+'%22]&endkey=[%22'+symbol+'%22,%22'+today+'%22]');
	$.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse_stock?startkey=[%22'+symbol+'%22,%22'+timeframe+'%22]&endkey=[%22'+symbol+'%22,%22'+today+'%22]', function(url_data) {
		$.each(url_data, function (i,element) {


			if ($.isArray(element) === true) {
				sortArrayByTime(element);
				element.reverse();
				setStockData(element);
					// console(getStockData().value.updated);

					$.each(getStockData(), function (i,value) {

						if (duration === "today") {
							console.log("Setting name to "+value.value.name);
							
							var date = new Date(parseInt(value.value.updated));
							date2=date.toString("HH:mm");
							console.log("date2 in today "+date2);
							datalist.push(value.value.openVal);
							datelist.push(date2); 
							latestlist.push(value.value.latest);
						// changelist.push(parseFloat(value.value.change));
					} 
					if (duration === "month" || duration === "week") {
						date2 = dateConvert(value.value.updated);
						if (date1 === null) {
							// setName(value.value.name);
							date1 = dateConvert(value.value.updated);
							console.log("Updated from null, now: "+date1);
							console.log(value.value.openVal);
							datalist.push(value.value.openVal);
							datelist.push(date2); 
							changelist.push(parseFloat(value.value.change));
							latestlist.push(value.value.latest);

						} else if (date1 === date2) {

						} else {
							date1 = date2;		
							datalist.push(value.value.openVal);
							datelist.push(date2); 
							changelist.push(parseFloat(value.value.change));
							latestlist.push(value.value.latest);
						}
					}
				});
}
});

setDataList(datalist.reverse());
setDateList(datelist.reverse());
setLatestList(latestlist.reverse());
setChangeList(changelist.reverse());
chartPaintSelector();
if (firstDataFill === true) {
	fillInDataTable();
	firstDataFill = false;
}
});

}    



function dateConvert(timeString) {
	var date = new Date(parseInt(timeString));
	date=date.toString("MMM dd");
	return date;
}


function addElement(Symbol,Name,Change,Value) {
	var ni = document.getElementsByTagName('tbody').item(0);
	var numi = document.getElementById('theValue');
	var num = (document.getElementById('theValue').value -1)+ 2;
	numi.value = num;
	var newdiv = document.createElement('tr');
	newdiv.className='clickableRow';
	newdiv.setAttribute('onClick', 'hidelist("'+Symbol+'","today","'+Name+'");');
	// newdiv.addEventListener('click', hidelist(Symbol));
	cell = document.createElement("td");
	cellName = document.createElement("td");
	cellChange = document.createElement("td");
	cellValue = document.createElement("td");
	textnode = document.createTextNode(Symbol);
	textName = document.createTextNode(Name);
	textChange = document.createTextNode(Change);
	textValue = document.createTextNode(Value);
	cell.appendChild(textnode);
	cellName.appendChild(textName);
	cellChange.appendChild(textChange);
	cellValue.appendChild(textValue);
	newdiv.appendChild(cell);
	newdiv.appendChild(cellName);
	newdiv.appendChild(cellChange);
	newdiv.appendChild(cellValue);
	ni.appendChild(newdiv);
}


function fillInDataTable() {
	portfolioController();
	var tableRef = document.getElementById('currentdata');
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	console.log("getname is " +getName());
	var cn = document.getElementById('companyname').innerHTML=getName();

	var ni = document.getElementById('currentdata');
	var numi = document.getElementById('theValue');
	var num = (document.getElementById('theValue').value -1)+ 2;
	numi.value = num;
	var newdiv = document.createElement('tr');
	newdiv.className='clickableRow';

	// newdiv.addEventListener('click', hidelist(Symbol));
	cell = document.createElement("td");
	cellName = document.createElement("td");
	cellChange = document.createElement("td");
	cellValue = document.createElement("td");
	cellSave = document.createElement("td");
	textnode = document.createTextNode(Symbol);
	textName = document.createTextNode(getName());
	textChange = document.createTextNode(getChangeList().pop());
	textValue = document.createTextNode(getLatestList().pop());
	textSave = document.createTextNode("Save");
	cellSave.setAttribute('onClick', 'hidelist("'+Symbol+'","today");');
	cell.appendChild(textnode);
	cellName.appendChild(textName);
	cellChange.appendChild(textChange);
	cellValue.appendChild(textValue);
	cellSave.appendChild(textSave);
	newdiv.appendChild(cell);
	// newdiv.appendChild(cellName);
	newdiv.appendChild(cellChange);
	newdiv.appendChild(cellValue);
	// newdiv.appendChild(cellSave);
	ni.appendChild(newdiv);

}
function portfolioController() {
var sb = document.getElementById('save');
	if (isStockSaved(Symbol) === true) {
		console.log("Already in portfolio");
		sb.onclick = function() { deleteStock(Symbol);
		document.getElementById('save').innerHTML = "Save";
		}
		document.getElementById('save').innerHTML = "Remove";
	} else {
		console.log("Not saved");
		sb.onclick = function() { saveStock(Symbol);
		document.getElementById('save').innerHTML = "Remove";

		}
		document.getElementById('save').innerHTML = "Save";
	}
}


function isStockSaved(symbol) {
	if (localStorage[symbol] === undefined) {

		return false;
	}
	else {

		return true;
	}
}

function saveStock(symbol) {
	localStorage[symbol] = symbol;
	console.log("saved");
	portfolioController();
}

function deleteStock(symbol) {
	localStorage.removeItem(symbol);
	console.log("deleted "+isStockSaved(Symbol));
	portfolioController();
}

// function paintlinechart() {
// 	console.log(getDataList());
// 	var lineChartData = {
// 		labels : datelist,
// 		datasets : [
// 		{
// 			fillColor : "rgba(151,187,205,0.5)",
// 			strokeColor : "rgba(151,187,205,1)",
// 			pointColor : "rgba(151,187,205,1)",
// 			pointStrokeColor : "#fff",
// 			data : datalist
// 		}
// 		]

// 	}

// 	var myLine = new Chart(document.getElementById("canvas").getContext("2d")).Line(lineChartData);
// }

function paintlinechart() {
	var cl = datalist;
	diagramdata = [];
	for (var i = 0;i <= cl.length;i++) {
		diagramdata.push({day: getDateList()[i],value: datalist[i]});
	}
{
 $("#canvas").dxChart({
  dataSource: diagramdata,

  series: {
    argumentField: "day",
    valueField: "value",
    name: "Closing value history",
    type: "line",
    color: '#ffa500'
  }
});
}
}

// function paintbarchart2() {
// 	var cl = getChangeList();
// 	barcolorlist = [];
// 	for (var i = 0;i <= cl.length;i++) {
// 		if (cl[i] < 0 ) {
// 			cl[i] = Math.abs(cl[i]);
// 			barcolorlist.push("#DC3522");
// 		} else {
// 			barcolorlist.push("#374140");
// 		}
// 	}
// 	console.log(barcolorlist);
// 	console.log(getChangeList());
// 	var barChartData = {
// 		labels : getDateList(),
// 		datasets : [
// 		{
// 			fillColor : barcolorlist,
// 			strokeColor : "rgba(151,187,205,1)",
// 			data : cl
// 		}
// 		]
// 	}

// 	var bars = new Chart(document.getElementById("canvas").getContext("2d")).Bar(barChartData);

// }
function paintbarchart() {
	var cl = getChangeList();
	diagramdata = [];
	for (var i = 0;i <= cl.length;i++) {
		diagramdata.push({day: getDateList()[i],value: getChangeList()[i]});
	}
	console.log(diagramdata);
{
 $("#canvas").dxChart({
  dataSource: diagramdata,

  series: {
    argumentField: "day",
    valueField: "value",
    name: "The daily change",
    type: "bar",
    color: '#ffa500'
  }
});
}
}


function chartPaintSelector() {
	console.log("chartPaintSelector "+getChartType());
	if(getChartType() === "bar") {
		paintbarchart();
	}
	if (getChartType() === "line") {
		paintlinechart();
	}
	// con = document.querySelector("#canvas");
	// chart = document.querySelector("#barcanvas");
	// chart.className = 'visuallyhidden';
	// con.className = 'visible';
}
function hideLine() {

	// con = document.querySelector("#canvas");
	// chart = document.querySelector("#barcanvas");
	// con.className = 'visuallyhidden';
	// chart.className = 'visible';
	paintbarchart();

}



function hidelist(Symbol,timeframe,name) {
	setSymbol(Symbol);
	con = document.querySelector("#listdisp");
	chart = document.querySelector("#statView");
	con.className = 'visuallyhidden';
	chart.className = 'visible';
	radiobtn = document.getElementById("day");
	radiobtn.checked = true;
	setChartType("line");
	firstDataFill = true;
	getstockhistory(Symbol,timeframe,name);

}


function showlist() {
	con = document.querySelector("#listdisp");
	chart = document.querySelector("#statView");
	datalist = [0];
	datelist = [0];
	chart.className = 'visuallyhidden';
	con.className = 'visible';

}

function sortArrayByTime(data) {
	data.sort(function (a, b) {
		a = a.value.updated;
		b = b.value.updated;
		return a.localeCompare(b);
	});
}

function sortArrayBySymbol(data) {
	data.sort(function (a, b) {
		a = a.value.symbol;
		b = b.value.symbol;
		return a.localeCompare(b);
	});
}




// $(function ()  
// {
//  $("#canvas").dxChart({
//   dataSource: [
//   {day: "Monday", oranges: 3},
//   {day: "Tuesday", oranges: 2},
//   {day: "Wednesday", oranges: -7},
//   {day: "Thursday", oranges: 43},
//   {day: "Friday", oranges: 6},
//   {day: "Saturday", oranges: 11},
//   {day: "Sunday", oranges: 4} ],

//   series: {
//     argumentField: "day",
//     valueField: "oranges",
//     name: "My oranges",
//     type: "bar",
//     color: '#ffa500'
//   }
// });
// }

// );