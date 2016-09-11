#NAME: MIRAJ ALAM  ID: 108757323
#IMPORTANT NOTE FOR GRADING: ALL THREE TEST CASES WORK
#HW # 6
#PAUL FODOR
#DUE DATE: 5/2/16
import sys
import tpg
import copy

deepHelper = 0
variables = {}
functions = {}
stackFrames= [];
blocks = 0
blockToSkip = 0
executeBlock = True
returnValue = 0
stackFrames.append(variables)
foundRetValue = False

class SemanticError(Exception):
    """
    This is the class of the exception that is raised when a semantic error
    occurs.
    """
    
# These are the nodes of our abstract syntax tree.
class Node(object):
    """
    A base class for nodes. Might come in handy in the future.
    """

    def evaluate(self):
        """
        Called on children of Node to evaluate that child.
        """
        raise Exception("Not implemented.")

class IntLiteral(Node):
    """
    A node representing integer literals.
    """

    def __init__(self, value):
        self.value = int(value)

    def evaluate(self):
        return self.value

class RealLiteral(Node):
    """
    A node representing integer literals.
    """

    def __init__(self, value):
        self.value = float(value)

    def evaluate(self):
        return self.value		

class StringLiteral(Node):
    """
    A node representing integer literals.
    """

    def __init__(self, value):
        self.value = str(value)[1:len(str(value))-1]   #IS THIS ALLOWED

    def evaluate(self):
        return self.value
class Identifier(Node):
    """
    A node representing integer literals.
    """

    def __init__(self, value):
        self.value = str(value)

    def l_evaluate(self):
        #print(self.value)
        return self.value
    def evaluate(self): #this is r_evaluate
        #print(self.value)
        global stackFrames
        #print("Winter is coming")
        #print(stackFrames)
        if (isinstance(stackFrames[-1][self.value],Identifier)):
            #print("cool cool cool");
            return ( stackFrames[-2][(stackFrames[-1][self.value]).value  ] )
        #print(stackFrames[-1][self.value])
        return stackFrames[-1][self.value]		
#modify this class a lot
class Assign(Node):
    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right
    def execute(self):
        global stackFrames
        if isinstance(self.left,Index):
            self.left.left.evaluate()[self.left.right.evaluate()] = self.right.evaluate()
            return			
        else:			
            left = self.left.l_evaluate()
        right = self.right.evaluate()
        stackFrames[-1][left] = right

class DefineFunction():
    def __init__(self, name, formalParameters, body):
        # The nodes representing the left and right sides of this
        # operation.
        self.name = name
        self.formalParameters = formalParameters
        self.body = body
        #print("Defining function");
        #print(self.name.l_evaluate());
        functions[self.name.l_evaluate()] = (self.formalParameters.value, self.body)		
    def execute(self):
        return; 	
class InvokeFunction():
    def __init__(self, name, actualParameters):
        # The nodes representing the left and right sides of this
        # operation.
        self.name = name
        self.actualParameters = actualParameters
#        print(actualParameters.value)
        #print("Did I get here?")
    def execute(self):
    #    print("In execute")	
        #execute the body

        #print(functions[self.name.l_evaluate()][0])
        #print(self.actualParameters.value)
        #print("Executing");
        #print(self.name.l_evaluate());
        global stackFrames

        #print(self.actualParameters.value)
        somecrap = copy.deepcopy(self.actualParameters)
 #       print(somecrap.value[0])
 #       print(self.actualParameters.value[0])	
        for i in  range(0,len(somecrap.value)):
            x = somecrap.value[i]
            if( not( isinstance(x,int) or isinstance(x,float)) ):		
                somecrap.value[i] = somecrap.value[i].evaluate()

  #      print("Got here")
  #      print(somecrap)
	    
        localVariables = dict(zip(functions[self.name.l_evaluate()][0],somecrap.value))
        #print(localVariables)
        stackFrames.append(localVariables)
        retValue = functions[self.name.l_evaluate()][1].evaluate()
        stackFrames.pop()
        return retValue
    def evaluate(self):
   #     print("In evaluate")
        #execute the body
        #print(functions[self.name.l_evaluate()][0])
        global stackFrames
         #print(self.actualParameters.value)
        somecrap = copy.deepcopy(self.actualParameters)
    #    print(somecrap.value[0])
    #    print(self.actualParameters.value[0])
        for i in  range(0,len(somecrap.value)):
            x = somecrap.value[i]
            if( not( isinstance(x,int) or isinstance(x,float)) ):		
                somecrap.value[i] = somecrap.value[i].evaluate()
       # print(self.actualParameters.value)		
     #   print("Got here")
     #   print(somecrap.value)
        localVariables = dict(zip(functions[self.name.l_evaluate()][0],somecrap.value))	
        #print(localVariables)
        stackFrames.append(localVariables)
        retValue = functions[self.name.l_evaluate()][1].evaluate()
        stackFrames.pop()
        return retValue
class FunctionBlock(Node):
    def __init__(self,block):
        self.block = block
        #print("init block")
        #self.returnVal = returnVal
    def evaluate(self):
        global foundRetValue
        global returnValue
		#print("This is block execute")sssss
        self.block.execute()
        foundRetValue = False;
        x = returnValue
        returnValue = 0		
        return x
        #return self.returnVal.execute()
class BoolOr(Node):
    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right
    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if left !=0 and left != 1: 
            raise SemanticError()
        if right !=0 and right != 1: 
            raise SemanticError()
        return left or right

class BoolAnd(Node):
    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right
    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if left !=0 and left != 1: 
            raise SemanticError()
        if right !=0 and right != 1: 
            raise SemanticError()
        return left and right
class BoolIn(Node):
    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right
    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if left in right: 
            return 1
        else:
            return 0
		
class BoolNot(Node):
    def __init__(self,right):
        # The nodes representing the left and right sides of this
        # operation.
        self.right = right
    def evaluate(self):
        right = self.right.evaluate()
        if not isinstance(right, int):
            raise SemanticError()
        if right == 0:
            return 1
        else:
            return 0
class ReturnStatement(Node):
    def __init__(self,retValue):
        self.retValue = retValue
    def execute(self):
        global deepHelper	
        retValue = self.retValue.evaluate();
        if(retValue != None):
            deepHelper = retValue
        if(retValue == None):
            retValue = deepHelper		
        #print(retValue)
        global foundRetValue;
        global returnValue;
        returnValue = retValue
        foundRetValue = True;
        #print("Returned atm");
        #print("Got here?")
        return retValue
class Block(Node):
    def __init__(self,right):
        self.right = right
        #print("init block")
    def execute(self):
        #print("This is block execute")
        self.right.execute()        
class Statement(Node):
    def __init__(self, statement, nextstatement):
        self.statement = statement
        self.nextstatement = nextstatement
        #print("Reached here in statement init")
    def execute(self):
        global returnValue
        #print("Reached here in statement execute");
        n = self.statement.execute()
        if(foundRetValue == False):		
            m = self.nextstatement.execute()
            if (foundRetValue == True):			
                #print("TF")
                #print(m)
                returnValue = m
                #print("End TF")
                if(returnValue == None):
                    returnValue = deepHelper    				
        else:
            #print("Respek me")
            #print(returnValue)
            return			
class IfStatement(Node):
    def __init__(self,right,block, elseBlock):
        self.right = right
        self.block = block
        self.elseBlock = elseBlock
    def execute(self):		
        right = self.right.evaluate()
        if(right == 0):
            if self.elseBlock == -1:
                #print("No else block")
                return
            else:
                #print("Will execute else block")			
                self.elseBlock.execute()
        else:				
            #print("If condition is true will execute block");
            self.block.execute()		

class WhileStatement(Node):
    def __init__(self,condition,block):
        self.condition = condition
        self.block = block
    def execute(self):		
        while(self.condition.evaluate()):
            self.block.execute()
class Print(Node):
    def __init__(self,right):
        # The nodes representing the left and right sides of this
        # operation.
        global executeBlock
        self.right = right
        #print("Init Print");
    def execute(self):	
        global executeBlock	
        right = self.right.evaluate()
        print(right,end ="")
        #print(right)
class PrintLn(Node):
    def __init__(self,right):
        # The nodes representing the left and right sides of this
        # operation.
        global executeBlock
        self.right = right
        #print("Init Print");
    def execute(self):	
        global executeBlock	
        right = self.right.evaluate()
        print(right)		
class GetEmptyList(Node):
    def __init__(self):
         self.value = []
		 #this is a commenst
    def evaluate(self):
        return self.value
			
class Comparison(Node):
    def __init__(self, left, right,op):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right
        self.op = op
    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        op = self.op
        if op == "==":
            if left == right:
                return 1
            else:
                return 0
        elif op == ">":
            if left > right:
                return 1
            else:
                return 0
        elif op == "<":
            if left < right:
                return 1
            else:
                return 0
        elif op == ">=":
            if left >= right:
                return 1
            else:
                return 0
        elif op == "<=":
            if left <= right:
                return 1
            else:
                return 0
        elif op == "<>":
            if left != right:
                return 1
            else:
                return 0				
        else:
            raise SemanticError();	
class Add(Node):
    """
    A node representing addition.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if( (isinstance(left,int)and isinstance(right,str)) or (isinstance(right,int)and isinstance(left,str)) ):
            raise SemanticError()		
        if not (isinstance(left, int) or isinstance(left, float) or isinstance(left, str)) :
            raise SemanticError()
        if not (isinstance(right, int) or isinstance(right, float) or isinstance(right, str)):
            raise SemanticError()
        return left + right

class Subtract(Node):
    """
    A node representing addition.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)):
            raise SemanticError()
        if not (isinstance(right, int) or isinstance(right, float)):
            raise SemanticError()
        return left - right		

class FloorDivide(Node):
    """
    A node representing division.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)):
            raise SemanticError()
        if not (isinstance(right, int) or isinstance(right, float)):
            raise SemanticError()
        if right == 0:
            raise SemanticError("His palms are sweaty")
        return left // right
		
class Multiply(Node):
    """
    A node representing multiplication.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)):
            raise SemanticError()
        if not (isinstance(right, int) or isinstance(right, float)):
            raise SemanticError()
        return left * right		
class Index(Node):
    """
    A node representing multiplication.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(right, int)):
            raise SemanticError()
        return left[right]
		
class Divide(Node):
    """
    A node representing division.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)):
            raise SemanticError()
        if not (isinstance(right, int) or isinstance(right, float)):
            raise SemanticError()
        if right == 0:
            raise SemanticError("His palms are sweaty")
        return left / right

class Exponent(Node):
    """
    A node representing division.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)):
            raise SemanticError()
        if not (isinstance(right, int) or isinstance(right, float)):
            raise SemanticError()
        return left ** right
		
class Modulus(Node):
    """
    A node representing division.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)):
            raise SemanticError()
        if not (isinstance(right, int) or isinstance(right, float)):
            raise SemanticError()
        if right == 0:
            raise SemanticError("His palms are sweaty.")
        return left % right	
# This is the TPG Parser that is responsible for turning our language into
# an abstract syntax tree.

# Note to self, had a big error where array elements could only be values and not expression that can be evaluated
#fixed by chaning b.value to b.evaluate() furthermore, b.evaluate isn't good enough, it will print something like 
#this is a method, instead of invoking and returning the method.
class Parser(tpg.Parser):
    """
	separator space '\s+';
	
	token real "(\d+\.\d*|\d*\.\d+)([eE][-+]?\d+)?|\d+[eE][-+]?\d+" RealLiteral;
	token int "\d+" IntLiteral;
	token str '\"[^\"]*\"' StringLiteral
	token identifier '[A-Za-z][A-Za-z0-9_]*' Identifier;
    
	

    START/a -> statementList/a | statement/a
    ;
	
	block/a -> ( "\{"statementList/a "\}"  $ a = Block(a) $ )
	;
	
	functionblock/a -> ( "\{"statementList/a "\}"  $ a = FunctionBlock(a) $ )
	;
	
	statementList/a ->((statement/a) (statement/b $a = Statement(a,b)$)*)
	| (statement/a)
	;
	
	statement/a -> (functionDef/a | println/a";" | print/a";" |assign/a";" 
	| block/a | ifStatement/a |whileStatement/a  | returnStatement/a";"  | expression/a";") 
	;
	
	returnStatement/a -> "return " expression/b $ a = ReturnStatement(b) $;
	 
	functionDef/a ->
	(identifier/a (formalParameterList/b)(functionblock/c)   $a = DefineFunction(a,b,c)$)
	;
	
	functionInvoke/a ->
	(identifier/a (actualParameterList/b)   $a = InvokeFunction(a,b)$)
	;
	
	ifStatement/a -> 
	("if"expression/b block/c "else" block/d $ a = IfStatement(b,c,d) $)
	| ("if"expression/b block/c $ a = IfStatement(b,c,-1) $)
	; 
	
	whileStatement/a -> 
	("while"expression/b block/c $ a = WhileStatement(b,c) $)
	;
    
	println/a -> ("println\("expression/b"\)" $ a = PrintLn(b) $)
	;
	
	print/a ->("print\("expression/b"\)" $ a = Print(b) $)
	;

	expression/a -> boolOr/a 
    ;
	boolOr/a -> boolAnd/a
    ( "or" boolAnd/b $ a = BoolOr(a, b) $
    )* ;

	boolAnd/a -> boolNot/a
    ( (("and")|("&&")) boolNot/b $ a = BoolAnd(a, b) $
    )*; 

	boolNot/a -> comparison/a |
     "not" expression/b $ a = BoolNot(b) $;
	
	comparison/a -> boolIn/a
    ( "==" boolIn/b $ a = Comparison(a, b, "==") $
	|">=" boolIn/b $ a = Comparison(a, b, ">=") $
	|"<=" boolIn/b $ a = Comparison(a, b, "<=") $
	|"<>" boolIn/b $ a = Comparison(a, b, "<>") $
	|"<" boolIn/b $ a = Comparison(a, b, "<") $
	|">" boolIn/b $ a = Comparison(a, b, ">") $
    )* ;
    
	boolIn/a -> addsub/a
    ( "in" addsub/b $ a = BoolIn(a, b) $
    )* ;
	
	addsub/a -> floordiv/a
    ( "\+" floordiv/b $ a = Add(a, b) $
    | "\-" floordiv/b $ a = Subtract(a, b) $
    )* ;

	floordiv/a -> exponent/a
    ( "//" exponent/b $ a = FloorDivide(a, b) $
    )* ;
	
	exponent/a -> mod/a
    ( "\*\*" mod/b $ a = Exponent(a, b) $
    )* ;
	
	mod/a -> muldiv/a
    ( "\%" muldiv/b $ a = Modulus(a, b) $
    )* ;	
	
    muldiv/a -> index/a
    ( "\*" index/b $ a = Multiply(a, b) $
    | "/"  index/b $ a = Divide(a, b) $
    )* ;

	index/a -> parens/a ("\[" expression/b
	"\]" $ a = Index(a,b) $
	)*;
	

	
    parens/a -> "\(" expression/a "\)" | functionInvoke/a | identifier/a|  literal/a |array/a
    ;
	
	assign/a -> expression/a "=" expression/b $ a = Assign(a,b) $
	;
    literal/a -> str/a |real/a |int/a ;
	
	array/a -> "\[" $ a = GetEmptyList() $
		expression/b $ a.value.append(b.evaluate()) $
		("," expression/b $ a.value.append(b.evaluate()) $ )*
		"\]"
		| "\[" "\]" $ a = GetEmptyList()$  ;
	formalParameterList/a -> "\(" $ a = GetEmptyList() $
		identifier/b $ a.value.append(b.l_evaluate()) $
		("," identifier/b $ a.value.append(b.l_evaluate()) $ )*
		"\)"
		| "\(" "\)" $ a = GetEmptyList()$  ;
	actualParameterList/a -> "\(" $ a = GetEmptyList() $
		expression/b $ a.value.append(b) $
		("," expression/b $ a.value.append(b) $ )*
		"\)"
		| "\(" "\)" $ a = GetEmptyList()$  ;
	
    """

# Make an instance of the parser. This acts like a function.
parse = Parser()

# This is the driver code, that reads in lines, deals with errors, and
# prints the output if no error occurs.

# Open the file containing the input.
try:
    f = open(sys.argv[1], "r")
except(IndexError, IOError):
    f = open("input.txt", "r")
try:
    node = parse(f.read());
    # Try to execute
    result = node.execute();
except tpg.Error:
    print("SYNTAX ERROR");
except SemanticError:
    print("SEMANTIC ERROR");	
#print("Remove this and the next line before submitting")
#print(variables)
#print(functions)
#print("Stack:")
#print(stackFrames);

#we can define fucntions now, but not yet with formal parameters, uncomment these only if input file defines a fucntion named wapoa.
#functions['wapao'].execute()
#functions['wapao'].execute()
#functions['wapao'].execute()
if blocks < 0:
    print("Error, you had more closing blocks than starting blocks")
elif blocks > 0:
    print("Error, you had more starting blocks than ending blocks")	
f.close()
