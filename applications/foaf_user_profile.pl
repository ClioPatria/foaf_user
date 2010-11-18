/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
		   VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(foaf_user_profile, []).
:- use_bundle(html_page).
:- use_module(user(user_db)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- rdf_register_ns(foaf, 'http://xmlns.com/foaf/0.1/').

:- http_handler(root('user/foaf_profile_form'),	foaf_profile_form,   []).
:- http_handler(api(update_foaf_profile),	update_foaf_profile, []).

/** <module> Manage a FOAF profile
*/

%%	foaf_profile_form(+Request)
%
%	HTTP handler that creates a form  for editing the most important
%	FOAF attributes of a person.

foaf_profile_form(_Request) :-
	ensure_logged_on(User),
	user_property(User, url(UserURI)),
	authorized(write(user, UserURI)),
	reply_html_page(cliopatria(user),
			title('Edit FOAF profile'),
			[ h1('Edit FOAF profile'),
			  \foaf_profile_form(User)
			]).


foaf_profile_form(User) -->
	{ user_property(User, url(UserURI)),
	  foaf_set_defaults(User)
	},
	html(form(action(location_by_id(update_foaf_profile)),
		  table(class(form),
			[ \hidden(r, UserURI),
			  \p_input(UserURI, foaf:name, []),
			  \p_input(UserURI, foaf:nick, [disabled]),
			  \p_input(UserURI, foaf:mbox, []),
			  \p_input(UserURI, foaf:workInfoHomepage, []),
			  \form_submit('Update account')
			]))).

foaf_set_defaults(User) :-
	user_property(User, url(UserURI)),
	(   rdfs_individual_of(UserURI, foaf:'Person')
	->  true
	;   rdf_assert(UserURI, rdf:type, foaf:'Person')
	),
	(   rdf(UserURI, foaf:name, _)
	->  true
	;   user_property(User, realname(Name)),
	    rdf_assert(UserURI, foaf:name, literal(Name), UserURI)
	),
	(   rdf(UserURI, foaf:nick, _)
	->  true
	;   rdf_assert(UserURI, foaf:nick, literal(User), UserURI)
	).


p_input(URI, P0, Options) -->
	{ rdf_global_id(P0, P),
	  rdf_display_label(P, Label),
	  (   rdf(URI, P, Value)
	  ->  (   rdf_has(P, rdf:type, owl:'ObjectProperty')
	      ->  Text = Value
	      ;	  literal_text(Value, Text)
	      )
	  ->  Extra = [value(Text)|Options]
	  ;   Extra = Options
	  )
	},
	form_input(Label,
		   input([ name(P),
			   size(50)
			 | Extra
			 ])).


		 /*******************************
		 *	         API		*
		 *******************************/

%%	update_foaf_profile(+Request)
%
%	Handle update from foaf_profile_form/1.

update_foaf_profile(Request) :-
	http_parameters(Request,
			[ r(UserURI,
			    [ description('OpenID/FOAF URI of the user')
			    ])
			],
			[ form_data(Form)
			]),
	authorized(write(user, UserURI)),
	maplist(update_user(UserURI), Form),
	reply_html_page(cliopatria(user),
			title('FOAF profile updated'),
			[ h1('FOAF profile updated')
			]).


update_user(_, r=_) :- !.
update_user(UserURI, P=Value) :-
	rdf_equal(P, foaf:mbox), !,
	rdf_retractall(UserURI, P, _),
	(   sub_atom(Value, 0, _, _, 'mailto:')
	->  MBOX = Value
	;   atom_concat('mailto:', Value, MBOX)
	),
	rdf_assert(UserURI, P, MBOX, UserURI).
update_user(UserURI, P=Value) :-
	rdf_has(P, rdfs:isDefinedBy, foaf:''), !,
	rdf_retractall(UserURI, P, _),
	(   rdf_has(P, rdf:type, owl:'ObjectProperty')
	->  rdf_assert(UserURI, P, Value, UserURI)
	;   rdf_assert(UserURI, P, literal(Value), UserURI)
	).
update_user(_, P=_) :-
	existence_error(foaf_property, P).



