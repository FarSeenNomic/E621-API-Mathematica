(* ::Package:: *)

(* ::Input::Initialization:: *)
outputDeleteCell[z_]:=CellPrint[ExpressionCell[z,"Output",CellTags->"textout"]]
deleteDeleteCell[]:=(NotebookLocate["textout"];NotebookDelete[];);
mapViewer[fun_,data_List]:=Module[{max=Length[data],variatout={},temp},
DynamicModule[{perc=0},

temp=PrintTemporary[Dynamic[ProgressIndicator[perc]]];
variatout=If[Length[#]==0,{},#[[1]]]&@Reap[Do[
perc=curr/max;
Sow[fun@data[[curr]]];
,{curr,max}
]][[2]];
NotebookDelete[temp]
];

variatout
];

$getLimit=75;

getByID[id_String,1]:=getByID[id,1]=(Print["ScreenScrape soon to be Deprecated"];Import["https://e621.net/post/show/"~~id,"Source","UserAgent"->"Mathe1\\Imanton1"]);
getByID[id_String,2]:=getByID[id,2]=ImportString[URLFetch["https://e621.net/post/show.json?id="~~id,"UserAgent"->"Mathe1\\Imanton1"],"JSON"];
getByID[id_]:=getByID[id,2];

getMagicScores[id_String]:=ToExpression/@StringCases[
StringTake[getByID[id,1],Extract[StringPosition[getByID[id,1],"<span id=\"post-score-"|"</span>\n\n\t\t\t\n\n\t\t\t\n\t\t</li>"],{{1,1},{-1,-1}}]+10{-1,1}],
{
"<span id=\"post-score-"~~id~~___~~" title=\""~~a__~~" up, "~~b__~~" down\">"~~c__~~"</span>\n\n\t\t\t\n\n\t\t\t\n\t\t</li>":>{a,b,c}
}
][[1]];
getFavourites[id_String]:="fav_count"/.getByID[id,2];
getFavouriters[id_String]:=If[getFavourites[id]==0,{},StringCases[
StringSplit[
StringCases[
getByID[id,1],
{
"<li>Favorited by: <span id=\"favorited-by\">"~~a__~~"</a></span>"~~("</li>"|"<span ")
}
][[1]],
", "
],
{"<a href=\"/user/show/"~~userid__~~"\">"~~name__~~"</a>":>{name,userid}}
][[All,1]]];
getSmallImage[id_String]:=getSmallImage[id]=Import["preview_url"/.getByID[id,2]];
getImage[id_String]:=getImage[id]=Import["file_url"/.getByID[id,2]];

getTags[id_String]:=StringSplit["tags"/.getByID[id,2]," "];
getType[id_String]:="file_ext"/.getByID[id,2];
getStatus[id_String]:="status"/.getByID[id,2];
getRating[id_String]:="rating"/.getByID[id,2];
getScore[id_String]:="score"/.getByID[id,2];
getSource[id_String]:="source"/.getByID[id,2];
getArtist[id_String]:="artist"/.getByID[id,2];

(*I don't know what "n" is, but "s" is Unix Time, the number of seconds from 00:00:00 UTC on 1 January 1970
see: https://en.wikipedia.org/wiki/Unix_time*)

getUpDate[id_String]:=("s"/.("created_at"/.getByID[id,2]))+AbsoluteTime[{1970}];
idTomd5[id_String]:="md5"/.getByID[id,2];
md5Toid[md5_String]:=md5Toid[md5]="id"/.ImportString[URLFetch["https://e621.net/post/show.json?md5="~~md5],"JSON"];
getFullUploadsByName[name_]:=getFullUploadsByName[name]=Map[
(getByID[ToString["id"/.#],2]=#)&,
ImportString[URLFetch["https://e621.net/post/index.json?limit="~~IntegerString[$getLimit]~~"&tags=user%3A"~~URLEncode[name]~~""],"JSON"]
];
getUploadsByName[name_]:=ToString/@("id"/.getFullUploadsByName[name]);
getCommentsQ[id_]:="has_comments"/.getByID[id,2];
getComments[id_String]:=getComments[id]=Reverse[ImportString[URLFetch["https://e621.net/comment/index.json?post_id="~~id],"JSON"]];
getUploader[id_String]:="author"/.getByID[id,2];

numofuploadsByName[user_String]:=Length@getUploadsByName[user];
favourersByName[user_String]:=SortBy[Tally@Flatten[getFavouriters[#][[All,1]]&~mapViewer~getUploadsByName[user]],-#[[2]]&];
getUserByName[user_String,1]:=getUserByName[user,1]=Import["https://e621.net/user/show/"~~URLEncode[user],"Source"];
getUserByName[user_String,2]:=getUserByName[user,2]=ImportString[URLFetch["https://e621.net/user/index.json?name="~~URLEncode[user]],"JSON"];
getLevelByName[user_String]:="level"/.getUserByName[user,2][[1]];
getAgeByName[user_String]:="created_at"/.getUserByName[user,2][[1]];
getLimitByName[user_String]:=ToExpression/@StringCases[getUserByName[user,1],
"<span title='Base upload limit' style='cursor:help'>"~~base__~~"</span> + (<span title='Number of approved uploads' style='cursor:help'>"~~app__~~"</span> / 10) - (<span title='Number of deleted uploads' style='cursor:help'>"~~una__~~"</span> / 4) - <span title='Number of currently unapproved posts' style='cursor:help'>"~~dis__~~"</span> = <span style='font-weight:bold;'>"~~tot__~~"</span>\n\t\t\t\t\t\t\n":><|"Base"->base,"Approved"->app,"Deleted"->una,"Unapproved"->dis,"Total"->tot|>
][[1]];

getTag[tag_String]:=getTag[tag]=(
getTag["name"/.#]={#};#
)&/@ImportString[URLFetch["https://e621.net/tag/index.json?name="~~URLEncode[tag]],"JSON"];
getFullByTag[tag_String,page_]:=(getByID[ToString["id"/.#],2]=#)&/@ImportString[URLFetch["https://e621.net/post/index.json?tags="~~URLEncode[tag]~~"&limit="~~IntegerString[$getLimit]~~"&page="~~ToString[page]],"JSON"];
getByTag[tag_String,page_:1]:=Map[ToString,ReplaceAll["id"/.getFullByTag[tag,page],"id"->{}]];

getPool[]:=getPool["",1];
getPool[title_String]:=getPool[title,1];
getPool[page_Integer]:=getPool["",page];
getPool[title_String,page_Integer]:=getPool[title,page]=ImportString[URLFetch["https://e621.net/pool/index.json?page="<>ToString[page]<>"&query="<>title],"JSON"]

(*login="&login=USERNAME&password_hash=4627f09ba90432809e1324b";*)

E621API::login="The variable login is not defined. Some functions may not load properly.";
If[!StringQ[login],Message[E621API::login]];

changeSource[id_,newSource_]:=Module[{},
ImportString[
URLFetch[
"https://e621.net/post/update.json?id="~~id~~
login~~
"&post[source]="~~URLEncode[newSource]~~
"&reason="~~URLEncode["M"]
,"UserAgent"->"Mathe1\\Imanton1"
,"Method"->"POST"
]
,"JSON"]
];
addTags[id_,tags_String]:=Module[{oldtags="tags"/.ImportString[URLFetch["https://e621.net/post/show.json?id="~~id],"JSON"]},
{"success","reason"}/.ImportString[
URLFetch[
"https://e621.net/post/update.json?id="~~id~~
login~~
"&post[tags]="~~URLEncode[oldtags~~" "~~tags]~~
"&post[old_tags]="~~URLEncode[oldtags]~~
"&reason="~~URLEncode["M"]
,"UserAgent"->"Mathe1\\Imanton1"
,"Method"->"POST"
]
,"JSON"]
];
removeTags[id_,tags_String]:=Module[{oldtags="tags"/.ImportString[URLFetch["https://e621.net/post/show.json?id="~~id],"JSON"]},
{"success","reason"}/.ImportString[
URLFetch[
"https://e621.net/post/update.json?id="~~id~~
login~~
"&post[tags]="~~URLEncode[StringReplace[oldtags,(" "~~tags~~" ")->" "]]~~
"&post[old_tags]="~~URLEncode[oldtags]~~
"&reason="~~URLEncode["M"]
,"UserAgent"->"Mathe1\\Imanton1"
,"Method"->"POST"
]
,"JSON"]
];
tagSearch[tags_,before_String:""]:=ToString[
"id"/.(
(getByID[ToString["id"/.#],2]=#)&/@ImportString[
URLFetch[
"https://e621.net/post/index.json?limit=1&tags="~~URLEncode[tags]~~If[before=="","","&before_id="~~before]
]
,"JSON"
][[1]]
)
];

negateTag[tag_]:=If[StringStartsQ[tag,"-"],StringTake[tag,{2,-1}],"-"<>tag];
idQ[id_]:=StringQ[id]&&StringMatchQ[id,RegularExpression["\\d+"]];
