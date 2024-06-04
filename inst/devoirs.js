console.log("In fooooo devoirs.js")

function devoirsCollectEssays() {
  var essay_answers = [];
  var items = document.getElementsByClassName("devoirs-text");
  for (i = 0; i < items.length; i++) {
    //console.log("text entry" + i + "being handled");
    essay_answers[i] = {itemid: items[i].id, contents: items[i].value}
  }

  return essay_answers;
}

function devoirsGetDocID() {
  return document.getElementById("devoirs-docID").innerHTML
}

function devoirsCollectMC() {
    let mc_answers = [] // Hold the information
    var ele = document.getElementsByClassName("devoirs-mcq");
    var count = 0;
    for (i = 0; i < ele.length; i++) {
        if (ele[i].checked) {
          //console.log("Entering conditional.");
          let checked_one = ele[i];
          mc_answers[count++] = {itemid: checked_one.id, truth: checked_one.getAttribute("truth"), start: checked_one.value};
        }
    }
    return mc_answers;
}

console.log("About to define WebR")

function devoirsCollectWebR() {
  var chunk_contents = []; // placeholder for collecting webr items
  if (typeof qwebrCellDetails == "undefined") {
    // There aren't any webr chunks
    return chunk_contents;
  }
  var chunks = qwebrCellDetails;
  for (i = 0; i < chunks.length; i++) {
    chunk_contents[i] = {chunk_id: i, code: chunks[i].code};
  }

  return chunk_contents;
}

console.log("About to define devoirsSubmit")

function devoirsSubmit() {
  items = {docid: devoirsGetDocID(), MC: devoirsCollectMC(), Essays: devoirsCollectEssays(), WebR: devoirsCollectWebR()}

  navigator.clipboard.writeText(JSON.stringify(items));

  // summarize what's being collected
  var my_summary = "A summary of your collected answers: Fixed choice: " + items.MC.length + " Essays: " + items.Essays.length + " WebR chunks: " + items.WebR.length

  document.getElementById("devoirs_summary").innerHTML = my_summary;
}

console.log("Read devoirsSubmit()")

// Hint handling in Multiple choice

// Still have to add an on/off switch from options

function devoirs_setup_hintarea() {
  answers = document.getElementsByClassName("devoirs-mcq")
  for (i=0; i<answers.length; i++) answers[i].addEventListener('click', function(e){document.getElementById(e.target.name + "-hintarea").innerHTML = e.target.getAttribute("hint")})
}

 window.addEventListener("load", function() {
  answers = document.getElementsByClassName("devoirs-mcq")
  for (i=0; i<answers.length; i++) {
    if (answers[i].getAttribute("show_hints") == "TRUE") {
      answers[i].addEventListener('click', function(e){document.getElementById(e.target.name + "-hintarea").innerHTML = e.target.getAttribute("hint")})
    }
  }
})

console.log("Added hint summary.")
