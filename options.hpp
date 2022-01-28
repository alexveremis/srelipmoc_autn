#pragma once

#include <iostream>
#include <ctype.h>
#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <getopt.h>
#include <iomanip>
#include <string>
#include <vector>

#include "ast.hpp"
#include "parser.hpp"
#include "type_inference.hpp"
#include "symbols.hpp"

class Option
{
protected:
    bool activated = false;
    int val;
    std::string name, description;
    int has_arg;
    std::string xtraArg = "";
public:
    Option(int val, std::string name, std::string description, int has_arg = no_argument);
    int getVal()	{return val;}
    std::string getName()	{return name;}
    std::string getDescription()	{return description;}
    std::string getOptarg()	{return xtraArg;}
    void setOptarg(std::string s)	{xtraArg = s;}
    bool isActivated()	{return activated;}
    void activate()	{activated = true;}
    void deactivate()	{activated = false;}
};

class SOption : public Option
{
public:
    SOption(char c, std::string description);
};

class BOption : public Option
{
protected:
    static int count;
public:
    BOption(std::string name, std::string description, int has_arg = no_argument);
    struct option getStructOption()	{ return {name.c_str(), has_arg, NULL, val};}
};

class OptionsList
{
protected:
    std::vector<SOption *> sOptions;
    std::vector<BOption *> bOptions;
    Program *p;
public:
    OptionsList();
    void addSOption(SOption *s)	{sOptions.push_back(s);}
    void addBOption(BOption *l)	{bOptions.push_back(l);}
    struct option* getBOptionArray()	{
		struct option *arr = new struct option[bOptions.size()];
		for (int i = 0; i < (int)bOptions.size(); i++)
		{
			arr[i] = bOptions[i]->getStructOption();
		}
		return arr;		
	}
    void parseOptions(int argc, char **argv);
    void putXtraProgram(Program *p);
    void executeOptions();
};

extern OptionsList optionsList;
extern BOption compiledDone;