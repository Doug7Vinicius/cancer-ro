$(document).ready(function() {
  // Altere o ícone dos submenus
  $('.treeview-menu > li > a').each(function() {
    $(this).append(' >'); // Adiciona o ícone '>'
  });

  // Remove o ícone '>>' original
  $('.treeview-menu .fa-caret-right').remove();
});
