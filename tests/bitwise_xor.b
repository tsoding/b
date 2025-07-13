main() {
    extrn printf;
    auto a, b, result;
    
    // Basic XOR operations
    a = 5;  // 0101
    b = 3;  // 0011
    result = a ^ b;  // Should be 6 (0110)
    printf("5 ^ 3 = %d\n", result);
    
    // Compound assignment
    a ^= b;  // Should be 6
    printf("a ^= 3 = %d\n", a);
    
    // XOR with zero (identity)
    printf("5 ^ 0 = %d\n", 5 ^ 0);
    
    // XOR with self (zero)
    printf("5 ^ 5 = %d\n", 5 ^ 5);
    
    // Multiple XOR operations
    printf("1 ^ 2 ^ 4 = %d\n", 1 ^ 2 ^ 4);
    
    // XOR with larger numbers
    printf("255 ^ 128 = %d\n", 255 ^ 128);
    
    // XOR with negative numbers (if supported)
    printf("10 ^ -5 = %d\n", 10 ^ -5);
    
    return;
} 