using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TFYAiK_WPF.Model
{
    public static class LexicalScanner
    {
        public struct LexicalItem
        {
            public Codes lexicalCode;
            public string item;
            public int startPosition;
            public int endPosition;

            public LexicalItem(Codes code, string item, int startPosition, int endPosition)
            {
                this.lexicalCode = code;
                this.item = item;
                this.startPosition = startPosition;
                this.endPosition = endPosition;
            }

            public override string ToString()
            {
                return $"{startPosition}:{endPosition}  {item}: {lexicalCode}: {Convert.ToInt16(lexicalCode)}";
            }
        }

        public enum Codes
        {
            ErrorCode = -1,
            IdentifierCode = 1,         // {a-z, A_Z, _}*
            IntegerConstCode,           // {0-9}*
            DoubleConstCode,            // {0-9}*{.} & {0-9}*
            RelationalOpCode,               // .EQ. (=) | .NE. (!=) | .GT. (>)
            AdditiveOpCode, MultiplicateOpCode, // + - * /
            LogicalOpCode, LogicalConstantCode,
            NotOpCode,
            LeftParenCode, RightParenCode,
        }

        public static bool IsLogicalOp(Codes code)
        {
            switch (code)
            {
                case Codes.LogicalOpCode:
                    //case Codes.LogicalAndCode:
                    //case Codes.LogicalOrCode:
                    return true;
                default:
                    break;
            }
            return false;
        }

        private static Codes IsArithmOperator(string text)
        {
            if (text.Length > 1)
            {
                return Codes.ErrorCode;
            }

            switch (text)
            {
                case "+":
                case "-":
                    return Codes.AdditiveOpCode;
                case "*":
                case "/":
                    return Codes.MultiplicateOpCode;
                default:
                    return Codes.ErrorCode;
            }
        }

        private static Codes IsOperator(string text)
        {
            if (text == "")
            {
                return Codes.ErrorCode;
            }

            switch (text.ToUpperInvariant())
            {
                case ".EQ.":
                case ".NE.":
                case ".GT.":
                case ".GE.":
                case ".LT.":
                case ".LE.":
                    return Codes.RelationalOpCode;
                case ".NOT.":
                    return Codes.NotOpCode;
                case ".AND.":
                case ".OR.":
                    return Codes.LogicalOpCode;
                default: return Codes.ErrorCode;
            }
        }

        private static Codes IsNumber(string text)
        {
            if (text == "")
            {
                return Codes.ErrorCode;
            }

            int integerConst = 0;

            if (int.TryParse(text, out integerConst))
            {
                return Codes.IntegerConstCode;
            }
            // Проверяет первое число на то что это цифра. \ Checks if the first number is a digit
            if (Char.IsDigit(text[0]))
            {
                int i = 0;
                int countDot = 0;

                while (i < text.Length)
                {
                    if (text[i] == '.')
                    {
                        countDot++;         // Точка должна быть только одна \ There must be only one point
                    }
                    i++;
                }

                if (countDot == 1)
                {
                    return Codes.DoubleConstCode;
                }
            }
            // Если TryParse вернул false или в строке больше одной точки.
            // \ If TryParse returned false or there is more than one dot in the string.
            return Codes.ErrorCode;
        }

        private static Codes IsIdentifier(string text)
        {
            if (text == "")
            {
                return Codes.ErrorCode;
            }

            if (!Char.IsLetter(text[0]))
            {
                return Codes.ErrorCode;
            }
            else
            {
                foreach (char c in text)
                {
                    if (c != '_' && !Char.IsDigit(c) && !Char.IsLetter(c))
                    {
                        return Codes.ErrorCode;
                    }
                }
            }
            return Codes.IdentifierCode;
        }

        private static Codes IsBracket(string text)
        {
            if (text == "")
            {
                return Codes.ErrorCode;
            }

            switch (text)
            {
                case "(": return Codes.LeftParenCode;
                case ")": return Codes.RightParenCode;
                default: return Codes.ErrorCode;
            }
        }

        private static Codes IsLogicalConstant(string text)
        {
            switch (text.ToUpper())
            {
                case "TRUE":
                case "FALSE":
                    return Codes.LogicalConstantCode;
                default:
                    return Codes.ErrorCode;
            }
        }

        private static char GetNext(string text, int currentPosition)
        {
            return text[currentPosition + 1];
        }

        public static List<LexicalItem> GetTokens(string inputString)
        {
            int i = 0;
            string answer = "";
            var parts = new List<LexicalItem>();
            string subString = "";

            while (i < inputString.Length)
            {
                char c = inputString[i];

                // Это скобки. \ It's brackets.
                if (c == ')' || c == '(')
                {
                    i++;
                    parts.Add(new LexicalItem(IsBracket(c.ToString()), c.ToString(), i, i));
                    continue;
                }

                // Может быть оператором. \ Can be an operator.
                if (c == '.')
                {
                    subString = "";
                    int start = i + 1;
                    int countDot = 0;

                    while ((i < inputString.Length - 1) && (char.IsLetter(inputString[i]) || inputString[i] == '.'))
                    {
                        if (countDot < 2)
                        {
                            subString += inputString[i];

                            if (inputString[i] == '.')
                            {
                                countDot++;
                            }
                        }
                        else
                        {
                            break;
                        }
                        i++;
                    }

                    if (subString.EndsWith("."))
                    {
                        //parts.Add($"{start}:{i}", IsOperator(subString));
                        parts.Add(new LexicalItem(IsOperator(subString), subString, start, i));
                    }

                    subString = "";
                }

                // Может быть идентификатором. \ Can be an identifier.
                if (Char.IsLetter(c))
                {
                    subString = "";
                    int start = i + 1;

                    while ((i < inputString.Length) && (Char.IsLetter(inputString[i]) || Char.IsDigit(inputString[i])))
                    {
                        subString += inputString[i];
                        i++;
                    }
                    if (IsLogicalConstant(subString) == Codes.LogicalConstantCode)
                    {
                        parts.Add(new LexicalItem(IsLogicalConstant(subString), subString, start, i));
                        continue;
                    }

                    parts.Add(new LexicalItem(IsIdentifier(subString), subString, start, i));
                }

                // Может быть числом. \ Can be a number.
                if (Char.IsDigit(c))
                {
                    subString = "";
                    int start = i + 1;

                    while ((i < inputString.Length) && (Char.IsDigit(inputString[i]) || inputString[i] == '.'))
                    {
                        subString += inputString[i];
                        i++;
                    }

                    if ((i < inputString.Length - 1) && Char.IsLetter(GetNext(inputString, i)))
                    {
                        subString = subString.Remove(subString.LastIndexOf('.'), 1);
                        i--;
                    }

                    if (subString.EndsWith("."))
                    {
                        //parts.Add($"{start}:{i}", Codes.ErrorCode);
                        parts.Add(new LexicalItem(Codes.ErrorCode, subString, start, i));
                    }
                    else
                    {
                        //parts.Add($"{start}:{i}", IsNumber(subString));
                        parts.Add(new LexicalItem(IsNumber(subString), subString, start, i));
                    }

                    subString = "";
                }

                // Это арифметический оператор. \ It is an arithmetic operator.
                if (c == '+' || c == '-' || c == '*' || c == '/')
                {
                    i++;
                    //parts.Add(i.ToString(), IsArithmOperator(c.ToString()));
                    parts.Add(new LexicalItem(IsArithmOperator(c.ToString()), c.ToString(), i, i));
                    continue;
                }
            }

            return parts;
        }
    }
}
