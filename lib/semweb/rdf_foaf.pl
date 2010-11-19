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

:- module(rdf_foaf,
	  [ foaf_mbox_hash/2,		% +MBOX, -Hash
	    foaf_add_mbox_hash/1,	% ?URI
	    foaf_merge/1		% ?URI
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sha)).

/** <module> FOAF reasoning

This module provides some  routines  to   support  reasoning  about FOAF
RDF.
*/

%%	foaf_merge(?URI) is det.
%
%	Create owl:sameAs links from other foaf   profiles that refer to
%	the same person.  The  reasoning  is   currently  based  on  the
%	foaf:mbox only. This first  uses   foaf_add_mbox_hash/1  to make
%	sure that all foaf:Person  instances   have  a foaf:mbox_sha1sum
%	property.
%
%	@param	URI is the foaf:Person for which to discover equivalents.
%		If this is omitted, equivalents are created for each
%		instance of foaf:Person.

foaf_merge(URI) :-
	foaf_add_mbox_hash(_),
	rdf_transaction(foaf_merge_raw(URI)).

foaf_merge_raw(URI) :-
	forall((rdf_has(URI, foaf:mbox_sha1sum, Hash),
		rdf_has(URI2, foaf:mbox_sha1sum, Hash, Graph),
		URI \== URI2),
	       rdf_assert(URI2, owl:sameAs, URI, Graph)).


%%	foaf_add_mbox_hash(?URI) is det.
%
%	Add foaf:mbox_sha1sum properties where they are not provided but
%	there is a foaf:mbox.
%
%	@param	URI is the URI of a foaf:Person.  If it is unbound, all
%		instances of foaf:Person are processed.

foaf_add_mbox_hash(URI) :-
	rdf_transaction(foaf_add_mbox_hash_raw(URI)).

foaf_add_mbox_hash_raw(URI) :-
	forall((rdf(URI, foaf:mbox, MBOX, DB),
		rdf_is_resource(MBOX),
		\+ rdf(URI, foaf:mbox_sha1sum, _)),
	       (foaf_mbox_hash(MBOX, Hash),
		rdf_assert(URI, foaf:mbox_sha1sum, literal(Hash), DB))).


%%	foaf_mbox_hash(+MBox, -Hash) is det.
%
%	Create a FOAF compatible hash  for   the  mailbox.  Note that it
%	seems    http://www.ldodds.com/foaf/foaf-a-matic    creates    a
%	case-sensitive hash. We create a   case-insensative hash because
%	E-mail addresses are cases-insensative.

foaf_mbox_hash(Mbox, Hash) :-
	downcase_atom(Mbox, Code),
	sha_hash(Code, Bytes, []),
	hex_codes(Bytes, Hex),
	atom_codes(Hash, Hex).

hex_codes([], []).
hex_codes([H|T0], [C1,C2|T]) :-
	V1 is H >> 4,
	V2 is H mod 0xf,
	code_type(C1, xdigit(V1)),
	code_type(C2, xdigit(V2)),
	hex_codes(T0, T).
