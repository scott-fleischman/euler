import java.util.*;
import java.lang.*;
import java.io.*;

public class problem_017
{
	static String getWordUnderTwenty(int numberUnderTwenty)
	{
		switch (numberUnderTwenty)
		{
			case 1: return "one";
			case 2: return "two";
			case 3: return "three";
			case 4: return "four";
			case 5: return "five";
			case 6: return "six";
			case 7: return "seven";
			case 8: return "eight";
			case 9: return "nine";
			case 10: return "ten";
			case 11: return "eleven";
			case 12: return "twelve";
			case 13: return "thirteen";
			case 14: return "fourteen";
			case 15: return "fifteen";
			case 16: return "sixteen";
			case 17: return "seventeen";
			case 18: return "eighteen";
			case 19: return "nineteen";
			default: return null;
		}
	}
	
	static String getTensWord(int tens)
	{
		switch(tens)
		{
			case 2: return "twenty";
			case 3: return "thirty";
			case 4: return "forty";
			case 5: return "fifty";
			case 6: return "sixty";
			case 7: return "seventy";
			case 8: return "eighty";
			case 9: return "ninety";
			default: return null;
		}
	}
	
	static String getWords(int number)
	{
		String result = null;
		if (number < 20)
		{
			result = getWordUnderTwenty(number);
		}
		else if (number < 100)
		{
			result = getTensWord(number / 10 % 10);
			String tens = getWordUnderTwenty(number % 10);
			if (tens != null)
				result += tens;
		}
		else if (number < 1000)
		{
			result = getWordUnderTwenty(number / 100 % 10);
			result += "hundred";
			String tens = getWords(number % 100);
			if (tens != null)
				result += "and" + tens;
		}
		else if (number < 10000)
		{
			result = getWordUnderTwenty(number / 1000 % 10);
			result += "thousand";
			String hundreds = getWords(number % 1000);
			if (hundreds != null)
				result += hundreds;
		}
		return result;
	}
	
	public static void main (String[] args) throws java.lang.Exception
	{
		int length = 0;
		for (int number = 1; number <= 1000; number++)
			length += getWords(number).length();
		System.out.println(length);
	}
}
