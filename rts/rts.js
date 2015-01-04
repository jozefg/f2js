var CURRENT_CLOS = null;
var CONT_STACK = [];
var ARG_STACK = [];
var EVAL_STACK = [];

var jumpNext = function(){
    return CONT_STACK.pop();
};

var mkClosure = function(shouldUpdate, closedVariables, body) {
    return {flag : shouldUpdate,
            clos : closedVariables,
            body : body};
};

var mkLit = function(x){
    return {flag : false,
            clos : [],
            body : function(){
                EVAL_STACK.push(x);
                return jumpNext();
            }
           };
};

var mkCon = function(tag, args){
    return mkLit({tag : tag, args : args});
};

var enter = function(c){
    CURRENT_CLOS = c;
    return c.body;
};

var evalFirst = function(){
    CURRENT_CLOS = ARG_STACK.pop();
    return enter(CURRENT_CLOS);
};

var project = function(s){
    return function(){
        var proj = EVAL_STACK.pop()[s];
        return enter(proj);
    };
};

var mkPrim = function(f){
    return function(){
        var r = EVAL_STACK.pop();
        var l = EVAL_STACK.pop();
        EVAL_STACK.push(f(l, r));
        return jumpNext();
    };
};

var plusPrim  = mkPrim(function(l, r){return l + r;});
var minusPrim = mkPrim(function(l, r){return l - r;});
var multPrim  = mkPrim(function(l, r){return l * r;});
var divPrim   = mkPrim(function(l, r){return l / r;});
var modPrim   = mkPrim(function(l, r){return l % r;});
var shlPrim   = mkPrim(function(l, r){return l << r;});
var shrPrim   = mkPrim(function(l, r){return l >> r;});
var eqPrim    = mkPrim(function(l, r){return l === r;});
var ltPrim    = mkPrim(function(l, r){return l < r;});
var ltePrim   = mkPrim(function(l, r){return l <= r;});
var gtPrim    = mkPrim(function(l, r){return l > r;});
var gtePrim   = mkPrim(function(l, r){return l >= r;});

var matchLit = function(x){
    return function(y){
        return x === y;
    };
};

var matchTag = function(t){
    return function(c){
        return c.tag === t;
    };
};

var matchAll = function(x){return true;};

var matcher = function(branches){
    return function(){
        var matchee = EVAL_STACK.pop();
        for(var x = 0; x < branches.length; ++x){
            if(branches[x].pred(matchee)){
                return branches[x].cont(matchee);
            }
        }
        throw "Match Failure";
    };
};

var terminal = function(){
    console.log(EVAL_STACK.pop());
};

var trampoline = function(c){
    var f = function(){return enter(c);}
    while(f){
        f = f();
    }
};

var enterMain = function(){
    CONT_STACK.push(terminal);
    trampoline(_main);
};
