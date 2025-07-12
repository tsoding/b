main() {
    auto x, y, z;
    
    // Test logical AND with short-circuit
    x = 0;
    y = 42;
    z = x && y;  // Should short-circuit, z = 0
    putchar(z + '0');  // Should print '0'
    
    x = 1;
    y = 0;
    z = x && y;  // Should not short-circuit, z = 0
    putchar(z + '0');  // Should print '0'
    
    x = 1;
    y = 1;
    z = x && y;  // Should not short-circuit, z = 1
    putchar(z + '0');  // Should print '1'
    
    // Test logical OR with short-circuit
    x = 1;
    y = 42;
    z = x || y;  // Should short-circuit, z = 1
    putchar(z + '0');  // Should print '1'
    
    x = 0;
    y = 0;
    z = x || y;  // Should not short-circuit, z = 0
    putchar(z + '0');  // Should print '0'
    
    x = 0;
    y = 1;
    z = x || y;  // Should not short-circuit, z = 1
    putchar(z + '0');  // Should print '1'
    
    auto nl;
    nl = 10;
    putchar(nl);
    return 0;
} 