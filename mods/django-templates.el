(require 'yasnippet)

(yas-define-snippets 'mhtml-mode '(
				   ("block" "{% block $1 %}%0{% endblock $1%}" "Django Templates")
				   ("extends" "{% extends '$1' %}$0" "Django Templates")
				   ("for" "{% for $1 in $2 %}$0{% endfor %}" "Django Templates")
				   ("{{" "{{ $1 }}$0" "Django Templates")
				   ("if" "{% if $1 %}$0" "Django Templates")))

(provide 'django-templates)
