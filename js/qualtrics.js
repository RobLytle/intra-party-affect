array = ["example1", "example2", "example3"];
for (i = 0; i < array.length; i++)
	document.getElementById("id_of_div").innerHTML += (i+1) + ": " + array[i];