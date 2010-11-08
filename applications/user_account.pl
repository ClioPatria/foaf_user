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

:- module(user_account, []).
:- use_bundle(html_page).
:- use_module(user(user_db)).

:- http_handler(root('user/edit_user'), edit_user_form, []).
:- http_handler(api(update_user),       update_user,    []).

/** <module> User account management
*/

edit_user_form(Request) :-
	http_parameters(Request,
			[ r(UserURI,
			    [ description('OpenID URI of the user')
			    ])
			]),
	authorized(write(user, UserURI)),
	reply_html_page(cliopatria(user),
			title('Edit user info'),
			[ h1('Edit user info'),
			  \user_form(UserURI)
			]).


user_form(UserURI) -->
	{ user_property(User, uri(UserURI)),
	  user_property(User, realname(RealName))
	},
	html(form(action(location_by_id(update_user)),
		  table(class(form),
			[ \hidden(r, UserURI),
			  \form_input('Real name',
				      input([ disabled,
					      value(RealName),
					      name(realname),
					      size(50)
					    ])),
			  \form_submit('Update account')
			]))).


		 /*******************************
		 *	         API		*
		 *******************************/

update_user(Request) :-
	http_parameters(Request,
			[ r(UserURI,
			    [ description('OpenID URI of the user')
			    ]),
			  realname(_RealName,
				   [ optional(true),
				     description('Real name of the user')
				   ])
			]),
	authorized(write(user, UserURI)).



