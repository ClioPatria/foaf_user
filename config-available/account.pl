:- module(conf_account, []).
:- use_module(account(applications/user_account)).
:- use_module(user(user_db)).
:- use_module(cliopatria(hooks)).

/** <module> Manage user accounts
*/

cliopatria:menu_popup_order(cpack, 250).
cliopatria:menu_label(cpack, 'CPACK').

cliopatria:menu_item(250=current_user/edit_user_form, 'My account') :-
	logged_on(_).
