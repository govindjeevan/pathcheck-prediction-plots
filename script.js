function toggleDesc() {
    console.log("Hi")
    var element = document.getElementsByClassName("desc")[0];
    element.classList.toggle("active");
    if (element.style.maxHeight) {
        element.style.maxHeight = null;
      } else {
        element.style.maxHeight = element.scrollHeight + "px";
      } 
  }