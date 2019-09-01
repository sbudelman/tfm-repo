var header = $('.navbar > .container-fluid');
header.append(
  '<ul class=\"nav navbar-nav navbar-right\">'+
    '<li><a href=\"https://www.github.com/sbudelman/tfm-repo\"><img style=\"height: 20px;\" alt=\"Github\" src=\"img/github.png\"></img></a></li>'+
    '<li class="dropdown">'+
        '<a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false"><img id=\"navLang\" style=\"height: 20px;\" alt=\"UK Flag\" src=\"img/united-kingdom.png\"></img></a>'+
        '<ul class="dropdown-menu">'+
            '<li id=\"langES\"><a href="#">ES<img style=\"height: 20px; margin-left: 5px;\" alt=\"Spain Flag\" src=\"img/spain.png\"></img></li>'+
            '<li id=\"langEN\"><a href="#">EN<img style=\"height: 20px; margin-left: 5px;\" alt=\"UK Flag\" src=\"img/united-kingdom.png\"></img></li>'+
          '</ul>'+
        '</li>'+
    '</ul>');
    
$('#langEN').click(function () {
  Shiny.setInputValue("selectedLang", "en");
  $('#navLang').attr('src', 'img/united-kingdom.png');
});

$('#langES').click(function () {
  Shiny.setInputValue("selectedLang", "es");
  $('#navLang').attr('src', 'img/spain.png');
});