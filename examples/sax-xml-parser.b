// SAX XML Parser 
//
// Simple XML-aware parser. CDATA is not supported.
//
// Usage: sax-xml-parser [OPTIONs] <input...>
// OPTIONS:
//    -f <file>
//        Parse content of the file
//    -s <str>
//        Parse input string
//    -e
//        Parse example string: <!DOCTYPE html><html><body><h1>My Heading</h1><p>My paragraph</p></body></html>
//    -t <file>
//        Extract text from a XML(HTML) file
//
// Written by Pavel Chumakou (pavel@chumakou.com)


parse(str, len, characters, start_tag, end_tag, start_document, end_document) 
{
    auto text_start; text_start = -1;
    auto tag_start; tag_start = -1;
    auto tag_name_end; tag_name_end = -1;        
    auto insideTag; insideTag = 0;
    
    (&*start_document)();
    
    auto i; i = 0; while(i < len) 
    {
        auto current; current = char(str, i);
        
        if (current == '<') 
        {
            
            if (insideTag == 1)
            {
                goto continue; // ... <xxx < ....  - skip this case
            }
            tag_start = i;
            tag_name_end = -1;
            insideTag = 1;
            goto continue;
        }
        
        if (current == ' ') 
        {
            if ((insideTag == 1) & (tag_name_end < 0)) 
            {
                tag_name_end = i;
            }
        }
        
        if (current == '>') 
        {
            if ((insideTag == 1) & (tag_start + 1 < i)) 
            {
                if (char(str, tag_start + 1) == '?' | char(str, tag_start + 1) == '!') 
                {
                    // skip <?xml > ... <!DOCTYPE>
                    text_start = -1;
                    tag_start = -1;
                    tag_name_end = -1;
                    insideTag = 0;
                    goto continue;
                }

                auto tag_name_start; tag_name_start = tag_start + 1;
                auto openTag; openTag = 1;
                if (char(str, tag_start + 1) == '/') 
                {
                    openTag = 0; // </xxx ...
                    tag_name_start++;
                }

                auto singleTag; singleTag = 0;
                auto prevByte; prevByte = char(str, i - 1);
                if (prevByte == '/') 
                {
                    singleTag = 1; // <xxx />
                    if (tag_name_end < 0) 
                    {
                        tag_name_end = i - 1;
                    }
                }

                if (tag_name_end < 0) 
                {
                    tag_name_end = i;
                }

                if (text_start >= 0) 
                {
                    (&*characters)(str, text_start, tag_start);
                    text_start = -1;
                }

                if (singleTag) 
                {
                    (&*start_tag)(str, tag_start, i + 1, tag_name_start, tag_name_end, 1);
                } else {
                    if (openTag) {
                        (&*start_tag)(str, tag_start, i + 1, tag_name_start, tag_name_end, 0);
                    } else {
                        (&*end_tag)(str, tag_start, i + 1, tag_name_start, tag_name_end);
                    }
                }

                tag_start = -1;
                tag_name_end = -1;
                insideTag = 0;
            }
            goto continue;
        }
        
        if (current == '\r' | current == '\n' | current == '\t') 
        {
            goto continue;
        }
        
        if (insideTag == 0) 
        {
            if (text_start < 0) {
                text_start = i;
            }
        }
        
continue:        
        i++;
    } // while
    
    if (text_start >= 0) {
        (&*characters)(str, text_start, len);
    }
    
    (&*end_document)();    
}

//// Parse event handlers /////////////

characters(str, start, end) 
{
    printf("characters: ");
    print_substring_n(str, start, end);
}

start_tag(str, tag_start, tagEnd, nameStart, nameEnd, isSelfClosing)
{
    printf("start_tag: ");
    print_substring_n(str, nameStart, nameEnd);
}

end_tag(str, tag_start, tagEnd, nameStart, nameEnd)
{
    printf("end_tag: ");
    print_substring_n(str, nameStart, nameEnd);
}

start_document() 
{
    printf("start_document\n");
}

end_document() 
{
    printf("end_document\n");
}

//// Extract text event handlers /////

ext_characters(str, start, end) 
{
    print_substring_n(str, start, end);
}

ext_start_tag(str, tag_start, tagEnd, nameStart, nameEnd, isSelfClosing){}

ext_end_tag(str, tag_start, tagEnd, nameStart, nameEnd){}

ext_start_document() {}

ext_end_document() {}

/////////////////////////////////

print_substring(str, start, end)
{
    auto i; i = start; while (i < end) 
    {
        putchar(char(str, i));
        i++;
    }
}

print_substring_n(str, start, end)
{
    print_substring(str, start, end);
    putchar('\n');
}

print_usage() 
{
    printf("SAX XML Parser\n");
    printf("Usage: sax-xml-parser [OPTIONS] <input...>\n");
    printf("OPTIONS:\n");
    printf("    -f <file>\n");
    printf("        Parse content of a file\n");
    printf("    -s \"<str>\"\n");
    printf("        Parse input string\n");
    printf("    -e  \n");
    printf("        Parse example string: <!DOCTYPE html><html><body><h1>My Heading</h1><p>My paragraph</p></body></html>\n");
    printf("    -t <file>\n");
    printf("        Extract text from a XML(HTML) file\n");        
}

buffer;
file_size;

read_file(fname)
{
    extrn fopen, ftell, fseek, fread, malloc;
    auto fp; fp = fopen(fname, "rb");
    if (fp == 0) 
    {
        printf("File %s not found.");
        exit(-1);
    }
    fseek(fp, 0, 2); // fseek(fp, 0, SEEK_END)
    file_size = ftell(fp);
    fseek(fp, 0, 0); //fseek(fp, 0, SEEK_SET);
    buffer = malloc(file_size);
    fread(buffer, 1, file_size, fp);  
}

main(argc, argv) 
{
    extrn malloc, strlen;
        
    if (argc <= 1) 
    {
        print_usage();
        return(0);
    }
    
    auto opt; opt = argv[1];
    if (char(opt, 0) == '-' & char(opt, 1) == 'f') 
    {
        if (argc <= 2) 
        {
            print_usage();
            return(0);
        }
        read_file(argv[2]);              
        parse(buffer, file_size, &characters, &start_tag, &end_tag, &start_document, &end_document);
        return(0);
    }
    
    if (char(opt, 0) == '-' & char(opt, 1) == 's') 
    {
        if (argc <= 2) 
        {
            print_usage();
            return(0);
        }        
        parse(argv[2], strlen(argv[2]), &characters, &start_tag, &end_tag, &start_document, &end_document);
        return(0);
    }
    
    if (char(opt, 0) == '-' & char(opt, 1) == 'e') 
    {       
        auto str; str = "<!DOCTYPE html><html><body><h1>My Heading</h1><p>My paragraph</p></body></html>";
        printf("Parsing example string: %s\n", str);
        parse(str, strlen(str), &characters, &start_tag, &end_tag, &start_document, &end_document);
        return(0);
    }
    
    if (char(opt, 0) == '-' & char(opt, 1) == 't') 
    {
        if (argc <= 2) 
        {
            print_usage();
            return(0);
        }        
        read_file(argv[2]);
        parse(buffer, file_size, &ext_characters, &ext_start_tag, &ext_end_tag, &ext_start_document, &ext_end_document);
        return(0);
    }
    
    print_usage();
    return(0);    

}

