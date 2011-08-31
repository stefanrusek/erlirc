%%% File    : numerics.erl
%%% Author  : Geoff Cant <nem@lisp.geek.nz>
%%% Description : translate irc numeric codes
%%% Created : 25 Mar 2006 by Geoff Cant <nem@lisp.geek.nz>

-module(irc_numerics).

-include_lib("irc.hrl").
-include_lib("logging.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([numeric_to_atom/1,
         atom_to_numeric/1,
         int_to_p10b64/1,
         int_to_p10b64/2,
         p10b64_to_int/1]).

numeric_to_atom("001") -> welcome;
numeric_to_atom("002") -> yourhost;
numeric_to_atom("003") -> created;
numeric_to_atom("004") -> myinfo;
%numeric_to_atom("005") -> map;
numeric_to_atom("005") -> isupport;
numeric_to_atom("006") -> mapmore;
numeric_to_atom("007") -> mapend;
numeric_to_atom("008") -> snomask;
numeric_to_atom("009") -> statmemtot;
numeric_to_atom("010") -> statmem;
numeric_to_atom("200") -> tracelink;
numeric_to_atom("201") -> traceconnecting;
numeric_to_atom("202") -> tracehandshake;
numeric_to_atom("203") -> traceunknown;
numeric_to_atom("204") -> traceoperator;
numeric_to_atom("205") -> traceuser;
numeric_to_atom("206") -> traceserver;
numeric_to_atom("208") -> tracenewtype;
numeric_to_atom("209") -> traceclass;
numeric_to_atom("211") -> statslinkinfo;
numeric_to_atom("212") -> statscommands;
numeric_to_atom("213") -> statscline;
numeric_to_atom("214") -> statsnline;
numeric_to_atom("215") -> statsiline;
numeric_to_atom("216") -> statskline;
numeric_to_atom("217") -> statsqline;
numeric_to_atom("218") -> statsyline;
numeric_to_atom("219") -> endofstats;
numeric_to_atom("220") -> statsbline;
numeric_to_atom("221") -> umodeis;
numeric_to_atom("222") -> sqline_nick;
numeric_to_atom("223") -> statsgline;
numeric_to_atom("224") -> statstline;
numeric_to_atom("225") -> statseline;
numeric_to_atom("226") -> statsnline;
numeric_to_atom("227") -> statsvline;
numeric_to_atom("231") -> serviceinfo;
numeric_to_atom("232") -> endofservices;
numeric_to_atom("233") -> service;
numeric_to_atom("234") -> servlist;
numeric_to_atom("235") -> servlistend;
numeric_to_atom("241") -> statslline;
numeric_to_atom("242") -> statsuptime;
numeric_to_atom("243") -> statsoline;
numeric_to_atom("244") -> statshline;
numeric_to_atom("245") -> statssline;
numeric_to_atom("246") -> statstline;
numeric_to_atom("247") -> statsgline;
numeric_to_atom("248") -> statsuline;
numeric_to_atom("249") -> statsdebug;
numeric_to_atom("250") -> luserconns;
numeric_to_atom("251") -> luserclient;
numeric_to_atom("252") -> luserop;
numeric_to_atom("253") -> luserunknown;
numeric_to_atom("254") -> luserchannels;
numeric_to_atom("255") -> luserme;
numeric_to_atom("256") -> adminme;
numeric_to_atom("257") -> adminloc1;
numeric_to_atom("258") -> adminloc2;
numeric_to_atom("259") -> adminemail;
numeric_to_atom("261") -> tracelog;
numeric_to_atom("262") -> endoftrace;
numeric_to_atom("265") -> n_local;
numeric_to_atom("266") -> n_global;
numeric_to_atom("271") -> silelist;
numeric_to_atom("272") -> endofsilelist;
numeric_to_atom("275") -> statsdline;
numeric_to_atom("280") -> glist;
numeric_to_atom("281") -> endofglist;
numeric_to_atom("290") -> helphdr;
numeric_to_atom("291") -> helpop;
numeric_to_atom("292") -> helptlr;
numeric_to_atom("293") -> helphlp;
numeric_to_atom("294") -> helpfwd;
numeric_to_atom("295") -> helpign;
numeric_to_atom("300") -> none;
numeric_to_atom("301") -> away;
numeric_to_atom("302") -> userhost;
numeric_to_atom("303") -> ison;
numeric_to_atom("304") -> rpl_text;
numeric_to_atom("305") -> unaway;
numeric_to_atom("306") -> nowaway;
numeric_to_atom("307") -> userip;
numeric_to_atom("308") -> rulesstart;
numeric_to_atom("309") -> endofrules;
numeric_to_atom("310") -> whoishelp;
numeric_to_atom("311") -> whoisuser;
numeric_to_atom("312") -> whoisserver;
numeric_to_atom("313") -> whoisoperator;
numeric_to_atom("314") -> whowasuser;
numeric_to_atom("315") -> endofwho;
numeric_to_atom("316") -> whoischanop;
numeric_to_atom("317") -> whoisidle;
numeric_to_atom("318") -> endofwhois;
numeric_to_atom("319") -> whoischannels;
numeric_to_atom("320") -> whoisvworld;
numeric_to_atom("321") -> liststart;
numeric_to_atom("322") -> list;
numeric_to_atom("323") -> listend;
numeric_to_atom("324") -> channelmodeis;
numeric_to_atom("329") -> channelcreate;
numeric_to_atom("331") -> notopic;
numeric_to_atom("332") -> topic;
numeric_to_atom("333") -> topicinfo;
numeric_to_atom("334") -> listusage;
numeric_to_atom("335") -> whoisbot;
numeric_to_atom("341") -> inviting;
numeric_to_atom("342") -> summoning;
numeric_to_atom("346") -> invitelist;
numeric_to_atom("347") -> endofinvitelist;
numeric_to_atom("348") -> exlist;
numeric_to_atom("349") -> endofexlist;
numeric_to_atom("351") -> version;
numeric_to_atom("352") -> whoreply;
numeric_to_atom("353") -> namreply;
numeric_to_atom("354") -> whospcrpl;
numeric_to_atom("361") -> killdone;
numeric_to_atom("362") -> closing;
numeric_to_atom("363") -> closeend;
numeric_to_atom("364") -> links;
numeric_to_atom("365") -> endoflinks;
numeric_to_atom("366") -> endofnames;
numeric_to_atom("367") -> banlist;
numeric_to_atom("368") -> endofbanlist;
numeric_to_atom("369") -> endofwhowas;
numeric_to_atom("371") -> info;
numeric_to_atom("372") -> motd;
numeric_to_atom("373") -> infostart;
numeric_to_atom("374") -> endofinfo;
numeric_to_atom("375") -> motdstart;
numeric_to_atom("376") -> endofmotd;
numeric_to_atom("377") -> motd2;
numeric_to_atom("378") -> austmotd;
numeric_to_atom("379") -> whoismodes;
numeric_to_atom("381") -> youreoper;
numeric_to_atom("382") -> rehashing;
numeric_to_atom("383") -> youreservice;
numeric_to_atom("384") -> myportis;
numeric_to_atom("385") -> notoperanymore;
numeric_to_atom("386") -> qlist;
numeric_to_atom("387") -> endofqlist;
numeric_to_atom("388") -> alist;
numeric_to_atom("389") -> endofalist;
numeric_to_atom("391") -> time;
numeric_to_atom("392") -> usersstart;
numeric_to_atom("393") -> users;
numeric_to_atom("394") -> endofusers;
numeric_to_atom("395") -> nousers;
numeric_to_atom("401") -> nosuchnick;
numeric_to_atom("402") -> nosuchserver;
numeric_to_atom("403") -> nosuchchannel;
numeric_to_atom("404") -> cannotsendtochan;
numeric_to_atom("405") -> toomanychannels;
numeric_to_atom("406") -> wasnosuchnick;
numeric_to_atom("407") -> toomanytargets;
numeric_to_atom("408") -> nosuchservice;
numeric_to_atom("409") -> noorigin;
numeric_to_atom("411") -> norecipient;
numeric_to_atom("412") -> notexttosend;
numeric_to_atom("413") -> notoplevel;
numeric_to_atom("414") -> wildtoplevel;
numeric_to_atom("416") -> querytoolong;
numeric_to_atom("421") -> unknowncommand;
numeric_to_atom("422") -> nomotd;
numeric_to_atom("423") -> noadmininfo;
numeric_to_atom("424") -> fileerror;
numeric_to_atom("425") -> noopermotd;
numeric_to_atom("431") -> nonicknamegiven;
numeric_to_atom("432") -> erroneusnickname;
numeric_to_atom("433") -> nicknameinuse;
numeric_to_atom("434") -> norules;
numeric_to_atom("435") -> serviceconfused;
numeric_to_atom("436") -> nickcollision;
numeric_to_atom("437") -> bannickchange;
numeric_to_atom("438") -> nicktoofast;
numeric_to_atom("439") -> targettoofast;
numeric_to_atom("440") -> servicesdown;
numeric_to_atom("441") -> usernotinchannel;
numeric_to_atom("442") -> notonchannel;
numeric_to_atom("443") -> useronchannel;
numeric_to_atom("444") -> nologin;
numeric_to_atom("445") -> summondisabled;
numeric_to_atom("446") -> usersdisabled;
numeric_to_atom("447") -> nonickchange;
numeric_to_atom("451") -> notregistered;
numeric_to_atom("455") -> hostilename;
numeric_to_atom("459") -> nohiding;
numeric_to_atom("460") -> notforhalfops;
numeric_to_atom("461") -> needmoreparams;
numeric_to_atom("462") -> alreadyregistered;
numeric_to_atom("463") -> nopermforhost;
numeric_to_atom("464") -> passwdmismatch;
numeric_to_atom("465") -> yourebannedcreep;
numeric_to_atom("466") -> youwillbebanned;
numeric_to_atom("467") -> keyset;
numeric_to_atom("468") -> invalidusername;
numeric_to_atom("469") -> linkset;
numeric_to_atom("470") -> linkchannel;
numeric_to_atom("471") -> channelisfull;
numeric_to_atom("472") -> unknownmode;
numeric_to_atom("473") -> inviteonlychan;
numeric_to_atom("474") -> bannedfromchan;
numeric_to_atom("475") -> badchannelkey;
numeric_to_atom("476") -> badchanmask;
numeric_to_atom("477") -> needreggednick;
numeric_to_atom("478") -> banlistfull;
numeric_to_atom("479") -> secureonlychannel;
numeric_to_atom("480") -> cannotknock;
numeric_to_atom("481") -> noprivileges;
numeric_to_atom("482") -> chanoprivsneeded;
numeric_to_atom("483") -> cantkillserver;
numeric_to_atom("484") -> ischanservice;
numeric_to_atom("485") -> killdeny;
numeric_to_atom("486") -> htmdisabled;
numeric_to_atom("489") -> secureonlychan;
numeric_to_atom("491") -> nooperhost;
numeric_to_atom("492") -> noservicehost;
numeric_to_atom("501") -> umodeunknownflag;
numeric_to_atom("502") -> usersdontmatch;
numeric_to_atom("511") -> silelistfull;
numeric_to_atom("513") -> badping;
numeric_to_atom("518") -> noinvite;
numeric_to_atom("519") -> admonly;
numeric_to_atom("520") -> operonly;
numeric_to_atom("521") -> listsyntax;
numeric_to_atom("524") -> operspverify;
numeric_to_atom("600") -> rpl_logon;
numeric_to_atom("601") -> rpl_logoff;
numeric_to_atom("602") -> rpl_watchoff;
numeric_to_atom("603") -> rpl_watchstat;
numeric_to_atom("604") -> rpl_nowon;
numeric_to_atom("605") -> rpl_nowoff;
numeric_to_atom("606") -> rpl_watchlist;
numeric_to_atom("607") -> rpl_endofwatchlist;
numeric_to_atom("610") -> mapmore;
numeric_to_atom("640") -> rpl_dumping;
numeric_to_atom("641") -> rpl_dumprpl;
numeric_to_atom("642") -> rpl_eodump;
numeric_to_atom("999") -> numericerror.


atom_to_numeric(welcome) -> "001";
atom_to_numeric(yourhost) -> "002";
atom_to_numeric(created) -> "003";
atom_to_numeric(myinfo) -> "004";
atom_to_numeric(map) -> "005";
atom_to_numeric(mapmore) -> "006";
atom_to_numeric(mapend) -> "007";
atom_to_numeric(snomask) -> "008";
atom_to_numeric(statmemtot) -> "009";
atom_to_numeric(statmem) -> "010";
atom_to_numeric(tracelink) -> "200";
atom_to_numeric(traceconnecting) -> "201";
atom_to_numeric(tracehandshake) -> "202";
atom_to_numeric(traceunknown) -> "203";
atom_to_numeric(traceoperator) -> "204";
atom_to_numeric(traceuser) -> "205";
atom_to_numeric(traceserver) -> "206";
atom_to_numeric(tracenewtype) -> "208";
atom_to_numeric(traceclass) -> "209";
atom_to_numeric(statslinkinfo) -> "211";
atom_to_numeric(statscommands) -> "212";
atom_to_numeric(statscline) -> "213";
atom_to_numeric(statsnline) -> "214";
atom_to_numeric(statsiline) -> "215";
atom_to_numeric(statskline) -> "216";
atom_to_numeric(statsqline) -> "217";
atom_to_numeric(statsyline) -> "218";
atom_to_numeric(endofstats) -> "219";
atom_to_numeric(statsbline) -> "220";
atom_to_numeric(umodeis) -> "221";
atom_to_numeric(sqline_nick) -> "222";
atom_to_numeric(statsgline) -> "223";
atom_to_numeric(statstline) -> "224";
atom_to_numeric(statseline) -> "225";
%atom_to_numeric(statsnline) -> "226";
atom_to_numeric(statsvline) -> "227";
atom_to_numeric(serviceinfo) -> "231";
atom_to_numeric(endofservices) -> "232";
atom_to_numeric(service) -> "233";
atom_to_numeric(servlist) -> "234";
atom_to_numeric(servlistend) -> "235";
atom_to_numeric(statslline) -> "241";
atom_to_numeric(statsuptime) -> "242";
atom_to_numeric(statsoline) -> "243";
atom_to_numeric(statshline) -> "244";
atom_to_numeric(statssline) -> "245";
%atom_to_numeric(statstline) -> "246";
%atom_to_numeric(statsgline) -> "247";
atom_to_numeric(statsuline) -> "248";
atom_to_numeric(statsdebug) -> "249";
atom_to_numeric(luserconns) -> "250";
atom_to_numeric(luserclient) -> "251";
atom_to_numeric(luserop) -> "252";
atom_to_numeric(luserunknown) -> "253";
atom_to_numeric(luserchannels) -> "254";
atom_to_numeric(luserme) -> "255";
atom_to_numeric(adminme) -> "256";
atom_to_numeric(adminloc1) -> "257";
atom_to_numeric(adminloc2) -> "258";
atom_to_numeric(adminemail) -> "259";
atom_to_numeric(tracelog) -> "261";
atom_to_numeric(endoftrace) -> "262";
atom_to_numeric(n_local) -> "265";
atom_to_numeric(n_global) -> "266";
atom_to_numeric(silelist) -> "271";
atom_to_numeric(endofsilelist) -> "272";
atom_to_numeric(statsdline) -> "275";
atom_to_numeric(glist) -> "280";
atom_to_numeric(endofglist) -> "281";
atom_to_numeric(helphdr) -> "290";
atom_to_numeric(helpop) -> "291";
atom_to_numeric(helptlr) -> "292";
atom_to_numeric(helphlp) -> "293";
atom_to_numeric(helpfwd) -> "294";
atom_to_numeric(helpign) -> "295";
atom_to_numeric(none) -> "300";
atom_to_numeric(away) -> "301";
atom_to_numeric(userhost) -> "302";
atom_to_numeric(ison) -> "303";
atom_to_numeric(rpl_text) -> "304";
atom_to_numeric(unaway) -> "305";
atom_to_numeric(nowaway) -> "306";
atom_to_numeric(userip) -> "307";
atom_to_numeric(rulesstart) -> "308";
atom_to_numeric(endofrules) -> "309";
atom_to_numeric(whoishelp) -> "310";
atom_to_numeric(whoisuser) -> "311";
atom_to_numeric(whoisserver) -> "312";
atom_to_numeric(whoisoperator) -> "313";
atom_to_numeric(whowasuser) -> "314";
atom_to_numeric(endofwho) -> "315";
atom_to_numeric(whoischanop) -> "316";
atom_to_numeric(whoisidle) -> "317";
atom_to_numeric(endofwhois) -> "318";
atom_to_numeric(whoischannels) -> "319";
atom_to_numeric(whoisvworld) -> "320";
atom_to_numeric(liststart) -> "321";
atom_to_numeric(list) -> "322";
atom_to_numeric(listend) -> "323";
atom_to_numeric(channelmodeis) -> "324";
atom_to_numeric(channelcreate) -> "329";
atom_to_numeric(notopic) -> "331";
atom_to_numeric(topic) -> "332";
atom_to_numeric(topicinfo) -> "333";
atom_to_numeric(listusage) -> "334";
atom_to_numeric(whoisbot) -> "335";
atom_to_numeric(inviting) -> "341";
atom_to_numeric(summoning) -> "342";
atom_to_numeric(invitelist) -> "346";
atom_to_numeric(endofinvitelist) -> "347";
atom_to_numeric(exlist) -> "348";
atom_to_numeric(endofexlist) -> "349";
atom_to_numeric(version) -> "351";
atom_to_numeric(whoreply) -> "352";
atom_to_numeric(namreply) -> "353";
atom_to_numeric(whospcrpl) -> "354";
atom_to_numeric(killdone) -> "361";
atom_to_numeric(closing) -> "362";
atom_to_numeric(closeend) -> "363";
atom_to_numeric(links) -> "364";
atom_to_numeric(endoflinks) -> "365";
atom_to_numeric(endofnames) -> "366";
atom_to_numeric(banlist) -> "367";
atom_to_numeric(endofbanlist) -> "368";
atom_to_numeric(endofwhowas) -> "369";
atom_to_numeric(info) -> "371";
atom_to_numeric(motd) -> "372";
atom_to_numeric(infostart) -> "373";
atom_to_numeric(endofinfo) -> "374";
atom_to_numeric(motdstart) -> "375";
atom_to_numeric(endofmotd) -> "376";
atom_to_numeric(motd2) -> "377";
atom_to_numeric(austmotd) -> "378";
atom_to_numeric(whoismodes) -> "379";
atom_to_numeric(youreoper) -> "381";
atom_to_numeric(rehashing) -> "382";
atom_to_numeric(youreservice) -> "383";
atom_to_numeric(myportis) -> "384";
atom_to_numeric(notoperanymore) -> "385";
atom_to_numeric(qlist) -> "386";
atom_to_numeric(endofqlist) -> "387";
atom_to_numeric(alist) -> "388";
atom_to_numeric(endofalist) -> "389";
atom_to_numeric(time) -> "391";
atom_to_numeric(usersstart) -> "392";
atom_to_numeric(users) -> "393";
atom_to_numeric(endofusers) -> "394";
atom_to_numeric(nousers) -> "395";
atom_to_numeric(nosuchnick) -> "401";
atom_to_numeric(nosuchserver) -> "402";
atom_to_numeric(nosuchchannel) -> "403";
atom_to_numeric(cannotsendtochan) -> "404";
atom_to_numeric(toomanychannels) -> "405";
atom_to_numeric(wasnosuchnick) -> "406";
atom_to_numeric(toomanytargets) -> "407";
atom_to_numeric(nosuchservice) -> "408";
atom_to_numeric(noorigin) -> "409";
atom_to_numeric(norecipient) -> "411";
atom_to_numeric(notexttosend) -> "412";
atom_to_numeric(notoplevel) -> "413";
atom_to_numeric(wildtoplevel) -> "414";
atom_to_numeric(querytoolong) -> "416";
atom_to_numeric(unknowncommand) -> "421";
atom_to_numeric(nomotd) -> "422";
atom_to_numeric(noadmininfo) -> "423";
atom_to_numeric(fileerror) -> "424";
atom_to_numeric(noopermotd) -> "425";
atom_to_numeric(nonicknamegiven) -> "431";
atom_to_numeric(erroneusnickname) -> "432";
atom_to_numeric(nicknameinuse) -> "433";
atom_to_numeric(norules) -> "434";
atom_to_numeric(serviceconfused) -> "435";
atom_to_numeric(nickcollision) -> "436";
atom_to_numeric(bannickchange) -> "437";
atom_to_numeric(nicktoofast) -> "438";
atom_to_numeric(targettoofast) -> "439";
atom_to_numeric(servicesdown) -> "440";
atom_to_numeric(usernotinchannel) -> "441";
atom_to_numeric(notonchannel) -> "442";
atom_to_numeric(useronchannel) -> "443";
atom_to_numeric(nologin) -> "444";
atom_to_numeric(summondisabled) -> "445";
atom_to_numeric(usersdisabled) -> "446";
atom_to_numeric(nonickchange) -> "447";
atom_to_numeric(notregistered) -> "451";
atom_to_numeric(hostilename) -> "455";
atom_to_numeric(nohiding) -> "459";
atom_to_numeric(notforhalfops) -> "460";
atom_to_numeric(needmoreparams) -> "461";
atom_to_numeric(alreadyregistered) -> "462";
atom_to_numeric(nopermforhost) -> "463";
atom_to_numeric(passwdmismatch) -> "464";
atom_to_numeric(yourebannedcreep) -> "465";
atom_to_numeric(youwillbebanned) -> "466";
atom_to_numeric(keyset) -> "467";
atom_to_numeric(invalidusername) -> "468";
atom_to_numeric(linkset) -> "469";
atom_to_numeric(linkchannel) -> "470";
atom_to_numeric(channelisfull) -> "471";
atom_to_numeric(unknownmode) -> "472";
atom_to_numeric(inviteonlychan) -> "473";
atom_to_numeric(bannedfromchan) -> "474";
atom_to_numeric(badchannelkey) -> "475";
atom_to_numeric(badchanmask) -> "476";
atom_to_numeric(needreggednick) -> "477";
atom_to_numeric(banlistfull) -> "478";
atom_to_numeric(secureonlychannel) -> "479";
atom_to_numeric(cannotknock) -> "480";
atom_to_numeric(noprivileges) -> "481";
atom_to_numeric(chanoprivsneeded) -> "482";
atom_to_numeric(cantkillserver) -> "483";
atom_to_numeric(ischanservice) -> "484";
atom_to_numeric(killdeny) -> "485";
atom_to_numeric(htmdisabled) -> "486";
atom_to_numeric(secureonlychan) -> "489";
atom_to_numeric(nooperhost) -> "491";
atom_to_numeric(noservicehost) -> "492";
atom_to_numeric(umodeunknownflag) -> "501";
atom_to_numeric(usersdontmatch) -> "502";
atom_to_numeric(silelistfull) -> "511";
atom_to_numeric(badping) -> "513";
atom_to_numeric(noinvite) -> "518";
atom_to_numeric(admonly) -> "519";
atom_to_numeric(operonly) -> "520";
atom_to_numeric(listsyntax) -> "521";
atom_to_numeric(operspverify) -> "524";
atom_to_numeric(rpl_logon) -> "600";
atom_to_numeric(rpl_logoff) -> "601";
atom_to_numeric(rpl_watchoff) -> "602";
atom_to_numeric(rpl_watchstat) -> "603";
atom_to_numeric(rpl_nowon) -> "604";
atom_to_numeric(rpl_nowoff) -> "605";
atom_to_numeric(rpl_watchlist) -> "606";
atom_to_numeric(rpl_endofwatchlist) -> "607";
%atom_to_numeric(mapmore) -> "610";
atom_to_numeric(rpl_dumping) -> "640";
atom_to_numeric(rpl_dumprpl) -> "641";
atom_to_numeric(rpl_eodump) -> "642";
atom_to_numeric(numericerror) -> "999";
atom_to_numeric(_) -> atom_not_numeric.

pow(_Num, 0) ->
    1;
pow(Num, Exp) ->
    Num * pow(Num, Exp - 1).


%%% P10 Base64

-define(P10Base64Alphabet, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789[]").
-define(P10AlphabetLength, length(?P10Base64Alphabet)).

int_to_p10b64(Int) ->
    int_to_p10b64_2(Int).

int_to_p10b64(Int, Len) when is_integer(Int) ->
    Str = int_to_p10b64_2(Int),
    int_to_p10b64(Str,Len - length(Str));
int_to_p10b64(Str, Len) when is_list(Str), Len > 0 ->
    "A" ++ int_to_p10b64(Str, Len - 1);
int_to_p10b64(Str, Len) when Len =< 0 ->
    Str.

int_to_p10b64_2(Int) when Int < 64  ->
    [lists:nth(Int + 1, ?P10Base64Alphabet)];
int_to_p10b64_2(Int) ->
    int_to_p10b64(Int rem 64) ++
        int_to_p10b64(Int div 64).

p10b64_to_int([C|Rest]) ->
    CharVal = string:chr(?P10Base64Alphabet, C) - 1,
    Places = length(Rest),
    (CharVal * pow(?P10AlphabetLength,Places)) + p10b64_to_int(Rest);
p10b64_to_int([]) ->
    0.

-ifdef(EUNIT).

int_to_p10b64_test() ->
    ?assert(int_to_p10b64(0,2) == "AA"),
    ?assert(int_to_p10b64(1,2) == "AB"),
    ?assert(int_to_p10b64(4095,2) == "]]").

p10b64_to_int_test() ->
    ?assert(p10b64_to_int("AA") == 0),
    ?assert(p10b64_to_int("A") == 0),
    ?assert(p10b64_to_int("AB") == 1),
    ?assert(p10b64_to_int("B") == 1),
    ?assert(p10b64_to_int("A]]") == 4095),
    ?assert(p10b64_to_int("]]") == 4095).

pow_test() ->
    ?assert(pow(64,0) == 1),
    ?assert(pow(64,1) == 64),
    ?assert(pow(64,2) == 4096).

-endif.





