/* Written by LDA (seija-amanojaku)
 * -------------------------------------------------------------------------------------------
 * Please note that this implementation assumes a twos-complement system for checking carries. 
 * If your system does not support twos complement(?!), please don't try to go too far from the 
 * original word length (signed).

 * A mostly OK, arbitary-integer calculator. Since the original dc is written in B, it is only 
 * fitting that this compiler also has its own version.
 * Here is a test macro(C) that takes the value onto the stack and applies one step of the Collatz conjecture 
 * (divide by two if even, otherwise multiply by 3 and add 1)
 *  - "[[lT2/]sE[lT3*1+]sOsTlT2%0=ElT2%0!=O]sC"
 * This code prints some numbers in the Fibonacci sequence (from https://esolangs.org/wiki/Dc) that are less than 5 * decimal digits (the 5 can be changed to any amount)
 *  - "1d[prdk+KdZ5>x]dsxx"
*/

WORD_LENGTH;
BWORD_LENGTH;
MAXWORD;
MAXWORD_ROOT;

LI_STRTYPE          1;
LI_NUMTYPE          0;

LI_TYPE             0;
LI_NUM_LENGTH       1;
LI_NUM_SIGN         2;
LI_NUM_DATASTART    3;      /* "Little" endian (in the way each word is stored, the overall endianness of 
                             * the words themselves doesn't matter) */

/* TODO: manage decimal points */
li_ctsize() {
    return (WORD_LENGTH);
}
li_ton(li) {
    /* Considering this function "casts" to a word, this is the best you ca get */
    if (li[LI_TYPE] == LI_NUMTYPE) {
        if (li[LI_NUM_SIGN])
            return (-li[LI_NUM_DATASTART]);
        else
            return (li[LI_NUM_DATASTART]);
    }
    return (0);
}
li_type(li) {
    return (li[LI_TYPE]);
}
li_iszero(li) {
    /* We ignore sign since -0 == +0 (can't believe I copied IEEE754) */
    if (li_type(li) == LI_NUMTYPE) {
        auto i; while (i < li[LI_NUM_LENGTH]) {
            if (li[LI_NUM_DATASTART+i] != 0) return (0);
            i++;
        }
    }
    return (1);
}
li_xtrct2(li, i) {
    extrn li_len2, li_xtrct2int;
    i = li_len2(li) - i - 1;
    return (li_xtrct2int(li, i));
}
li_xtrct2int(li, i) {
    extrn li_new, li_len2;
    auto woff, boff;

    woff = i / (BWORD_LENGTH - 1);
    boff = i % (BWORD_LENGTH - 1);
    if (li_type(li) == LI_NUMTYPE) {
        if (woff < li[LI_NUM_LENGTH])
        {
            return (li_new((li[LI_NUM_DATASTART+woff] >> boff) & 1));
        }
    }
    return (li_new(0));
}
li_setb2(li, i, b) {
    extrn li_new;
    auto woff, boff;

    woff = i / (BWORD_LENGTH - 1);
    boff = i % (BWORD_LENGTH - 1);
    if (li_type(li) == LI_NUMTYPE) {
        if (woff < li[LI_NUM_LENGTH]) {
            li[LI_NUM_DATASTART + woff] |= ((!!b) << boff);
        }
    }
}
li_len2(li) {
    extrn li_new, li_free, li_mul, li_lt;
    if (li_type(li) == LI_NUMTYPE) {
        auto i; i = li_new(1);
        auto r; r = 2;
        auto two; two = li_new(2);
        auto one; one = li_new(1);
        while (1) {
            auto next; next = li_mul(i, two);
            if (!li_lt(next, li)) {
                li_free(next);
                goto end;
            }
            li_free(i);
            i = next;
            r++;
        }
end:
        li_free(i);
        li_free(two);
        li_free(one);

        return (r);
    }
    return (0);
}
li_newlen2(len) {
    extrn li_newlen;
    auto full, part;
    full = len / BWORD_LENGTH;
    part = len % BWORD_LENGTH;
    if (part) part = 1;
    return (li_newlen(full + part));
}
li_newlen(len) {
    auto news, sgn;
    extrn malloc, memset;
    sgn = 0;

    news = malloc((LI_NUM_DATASTART + len) * WORD_LENGTH);
    news[LI_TYPE] = LI_NUMTYPE;
    news[LI_NUM_LENGTH] = len;
    news[LI_NUM_SIGN] = sgn;
    memset(&news[LI_NUM_DATASTART], 0, WORD_LENGTH * len);

    return (news);
}
li_new(nw) {
    auto news, sgn;
    extrn malloc;
    sgn = 0;

    if (nw < 0) { sgn = 1; nw = -nw; }

    news = malloc((LI_NUM_DATASTART + 1) * WORD_LENGTH);
    news[LI_TYPE] = LI_NUMTYPE;
    news[LI_NUM_LENGTH] = 1;
    news[LI_NUM_SIGN] = sgn;
    news[LI_NUM_DATASTART] = nw;

    return (news);
}
li_str(s) {
    auto len, news;
    extrn malloc, strlen, memcpy;

    len = strlen(s);
    news = malloc(len + 1 + WORD_LENGTH);
    news[LI_TYPE] = LI_STRTYPE;
    memcpy(news + WORD_LENGTH, s, len + 1);

    return (news);
}
li_show(li, b) {
    extrn printf, li_div, li_iszero, li_mod;
    extrn li_free, li_copy, putchar;
    if (li[LI_TYPE] == LI_NUMTYPE) {
        auto li1; li1 = li_copy(li);
        auto b1; b1 = li_new(b);

        if (li[LI_NUM_SIGN] == 1) printf("-");
        li1[LI_NUM_SIGN] = 0;
        auto a, c1, c;

        a = li_div(li1, b1);
        c1 = li_mod(li1, b1);
        if (!li_iszero(a)) 
            li_show(a, b);

        c = li_ton(c1) + '0';
        li_free(c1);
        li_free(a);
        li_free(b1);
        li_free(li1);

        if (c > '9') c += 7;
        putchar(c);
    } else
        printf("%s", &li[1]);
}
li_copy1(li) {
    extrn malloc, memcpy;
    if (li[LI_TYPE] == LI_NUMTYPE) {
        auto nli; nli = malloc((LI_NUM_DATASTART + li[LI_NUM_LENGTH]+1) * WORD_LENGTH);
        memcpy(nli, li, (LI_NUM_DATASTART + li[LI_NUM_LENGTH]) * WORD_LENGTH);
        nli[li[LI_NUM_LENGTH]+LI_NUM_DATASTART] = 0;
        nli[LI_NUM_LENGTH]++;
        return (nli);
    }
    return (li_str(&li[1]));
}
li_grow(li) {
    extrn malloc, memcpy, li_free;
    if (li[LI_TYPE] == LI_NUMTYPE) {
        auto nli; nli = malloc((LI_NUM_DATASTART + li[LI_NUM_LENGTH]+1) * WORD_LENGTH);
        memcpy(nli, li, (LI_NUM_DATASTART + li[LI_NUM_LENGTH]) * WORD_LENGTH);
        nli[li[LI_NUM_LENGTH]+LI_NUM_DATASTART] = 0;
        nli[LI_NUM_LENGTH]++;
        li_free(li);
        return (nli);
    }
}
li_copy(li) {
    extrn malloc, memcpy;
    if (li[LI_TYPE] == LI_NUMTYPE) {
        auto nli; nli = malloc((LI_NUM_DATASTART + li[LI_NUM_LENGTH]) * WORD_LENGTH);
        memcpy(nli, li, (LI_NUM_DATASTART + li[LI_NUM_LENGTH]) * WORD_LENGTH);
        return (nli);
    }
    return (li_str(&li[1]));
}
li_add(li1, li2) {
    if ((li1[0] == LI_NUMTYPE) & (li1[0] == LI_NUMTYPE)) {
        auto sgn, li;
        sgn = li1[LI_NUM_SIGN];
        if (li2[LI_NUM_SIGN] == 1) sgn = !sgn;

        if (sgn) {
            auto x, y;          /* x - y, depending on which one is negative */
            auto maxlen, smol, large;
            extrn memcpy;
            if (li1[LI_NUM_SIGN] == 1)  { x = li2; y = li1; }
            else                        { x = li1; y = li2; }

            { smol = li2; large = li1; maxlen = li1[LI_NUM_LENGTH]; }
            if (li2[LI_NUM_LENGTH] > maxlen) {
                maxlen = li2[LI_NUM_LENGTH];
                smol = li1;
                large = li2;
            }
            li = li_newlen(maxlen);
            memcpy(li, x, (LI_NUM_DATASTART + x[LI_NUM_LENGTH]) * WORD_LENGTH);
            auto i, carry; carry = 0; i = 0; while (i < maxlen) {
                auto xw, yw, sum;
                xw = 0; yw = 0;
                if (i < x[LI_NUM_LENGTH]) xw = x[LI_NUM_DATASTART + i];
                if (i < y[LI_NUM_LENGTH]) yw = y[LI_NUM_DATASTART + i];
                sum = xw - yw - carry;
                if (sum < 0) {
                    sum += MAXWORD + 1;
                    carry = 1;
                } else carry = 0;
                li[LI_NUM_DATASTART + i++] = sum;
            }
            if (carry) {
                li[LI_NUM_SIGN] = 1;
                i = 0; while (i < li[LI_NUM_LENGTH]) {
                    li[LI_NUM_DATASTART + i] = 
                        (MAXWORD + 1) - li[LI_NUM_DATASTART + i] - 
                        (i == 0 ? 0 : 1)
                    ;
                    i++;
                }
            } else li[LI_NUM_SIGN] = 0;
        } else {
            auto maxlen, smol, large;

            { smol = li2; large = li1; maxlen = li1[LI_NUM_LENGTH]; }
            if (li2[LI_NUM_LENGTH] > maxlen) {
                maxlen = li2[LI_NUM_LENGTH];
                smol = li1;
                large = li2;
            }

            li = li_copy(large);
            auto carry, i;
            i = 0; carry = 0; while (i < (smol[LI_NUM_LENGTH])) {
                auto w1; w1 = smol[LI_NUM_DATASTART + i];
                auto w2; w2 = large[LI_NUM_DATASTART + i];
                auto sum; sum = w1 + w2 + carry;

                /* Overflow will result in negative amounts. Negative symbols mean that the top bit is 
                 * set, thus, we can check if the sum is negative as a reasonable means to have a carry bit */
                if (sum < 0) {
                    carry = 1;

                    /* Take the result without any carry */
                    sum = sum & MAXWORD;
                } else carry = 0;
                li[LI_NUM_DATASTART+i++] = sum;
            }
            if (carry) {
                if (i >= li[LI_NUM_LENGTH])
                    li = li_grow(li);
                li[LI_NUM_DATASTART+i]++;
            }
        }

        return (li);
    }
    return (li_new(0));
}
li_sub(li1, li2) {
    if ((li1[0] == LI_NUMTYPE) & (li1[0] == LI_NUMTYPE)) {
        /* This function effectively piggybacks from the li_add implementation, which does check sign properly */
        extrn li_neg, li_free;
        auto diff;
        li2 = li_neg(li2);
        diff = li_add(li1, li2);
        li_free(li2);
        return (diff);
    }
    return (li_new(0));
}

/* TODO: This method of exp/multiplication is slow! */
li_exp(li1, li2) {
    if ((li1[0] == LI_NUMTYPE) & (li1[0] == LI_NUMTYPE)) {
        extrn li_free, li_lt, li_get, li_mul;
        auto sum;
        auto one;
        auto zero;

        sum = li_new(1);
        zero = li_new(0);
        one = li_new(1);
        li1 = li_copy(li1);
        li2 = li_copy(li2);

        li2[LI_NUM_SIGN] = 0;

        while (li_lt(zero, li2))
        {
            auto nsum; nsum = li_mul(sum, li1);
            li_free(sum);
            sum = nsum;

            auto ncount; ncount = li_add(zero, one);
            li_free(zero);
            zero = ncount;
        }

        if (li_iszero(sum)) sum[LI_NUM_SIGN] = 0;

        li_free(li1);
        li_free(li2);
        li_free(zero);
        li_free(one);
        return (sum);
    }
    return (li_new(0));
}
li_mul(li1, li2) {
    if ((li1[0] == LI_NUMTYPE) & (li1[0] == LI_NUMTYPE)) {
        extrn li_free, li_lt, li_get;
        auto sum;
        auto one;
        auto zero;
        auto sgn1, sgn2, sgn;

        sum = li_new(0);
        zero = li_new(0);
        one = li_new(1);

        if (li_get(li2, li1)) {
            auto li2t;
            li2t = li2;
            li2 = li1;
            li1 = li2t;
        }

        li1 = li_copy(li1);
        sgn1 = li1[LI_NUM_SIGN];
        li2 = li_copy(li2);
        sgn2 = li2[LI_NUM_SIGN];

        sgn = sgn1;
        if (sgn2) sgn = !sgn;

        li1[LI_NUM_SIGN] = 0;
        li2[LI_NUM_SIGN] = 0;

        while (li_lt(zero, li2))
        {
            auto nsum; nsum = li_add(sum, li1);
            li_free(sum);
            sum = nsum;

            auto ncount; ncount = li_add(zero, one);
            li_free(zero);
            zero = ncount;
        }

        sum[LI_NUM_SIGN] = sgn;
        if (li_iszero(sum)) sum[LI_NUM_SIGN] = 0;

        li_free(li1);
        li_free(li2);
        li_free(zero);
        li_free(one);
        return (sum);
    }
    return (li_new(0));
}
li_div(li1, li2) {
    if ((li1[0] == LI_NUMTYPE) & (li1[0] == LI_NUMTYPE)) {
        extrn li_get, li_add, li_sub, li_free, li_let, li_lt;
        auto quotient, one;
        auto sgn1, sgn2, sgn;
        
        // Effectively li1 XOR li2
        auto nsign; nsign = li1[LI_NUM_SIGN];
        if (li2[LI_NUM_SIGN] == 1) nsign = !nsign;
        one = li_new(1);
        li1 = li_copy(li1);
        li2 = li_copy(li2);
        sgn1 = li1[LI_NUM_SIGN];
        sgn2 = li2[LI_NUM_SIGN];
        li1[LI_NUM_SIGN] = 0;
        li2[LI_NUM_SIGN] = 0;

        sgn = sgn1;
        if (sgn2) sgn = !sgn;

        auto k; k = li_len2(li1);
        auto l; l = li_len2(li2);
        if (k < l) {
            // Quotient = 0, Remainder = li1
            quotient = li_new(0);
        } else {
            auto kl; kl = k - l + 1;
            auto q, r, i;
            auto b;

            // Initialisation
            b = li_new(2);
            q = li_new(0);
            r = li_newlen2(l-1);
            i = 0; while (i < (l-1)) {
                auto alpha_i; alpha_i = li_xtrct2(li1, i);
                auto spot; spot = l - 2 - i;
                if (li_ton(alpha_i)) {
                    li_setb2(r, spot, 1);
                }
                li_free(alpha_i);
                i++;
            }
            //r = li_new(8);
            i = 0; while (i < kl) {
                auto di, qi, ri, bi;
                auto br, mbi, bqi;

                // Compute d_i
                br = li_mul(b, r);
                di = li_add(br, li_xtrct2(li1, i+l-1));
                li_free(br);
                // Compute r_i
                // bi is the only number [0;1] s.t d_i-mb_i<m
                // Iff bi == 0, then d_i<m
                if (li_lt(di, li2)) {
                    bi = li_new(0);
                } else {
                    bi = li_new(1);
                }
                mbi = li_mul(bi, li2);
                ri = li_sub(di, mbi);

                // Finally, compute q_i
                bqi = li_mul(b, q);
                qi = li_add(bqi, bi);
                li_free(bqi);

                // Clean everything up
                li_free(q);
                q = qi;
                li_free(r);
                r = ri;

                i++;
                li_free(bi);
                li_free(di);
                li_free(mbi);
            }
            quotient = q;
            li_free(r);
        }

        li_free(li1);
        li_free(li2);
        li_free(one);

        quotient[LI_NUM_SIGN] = sgn;
        if (li_iszero(quotient)) quotient[LI_NUM_SIGN] = 0;

        return (quotient);
    }
    return (li_new(0));
}
li_mod(li_a, li_b) {
    auto div, divmul, res;
    extrn li_free;

    div = li_div(li_a, li_b);
    divmul = li_mul(div, li_b);
    res = li_sub(li_a, divmul);
    li_free(div);
    li_free(divmul);

    return (res);
}
li_gt(li1, li2) {
    if ((li1[0] == LI_NUMTYPE) & (li1[0] == LI_NUMTYPE)) {
        extrn li_let;
        return (!li_let(li1, li2));
    }
    return (0);
}
li_get(li1, li2) {
    /* TODO: Replace this with a proper bignum system */
    if ((li1[0] == LI_NUMTYPE) & (li1[0] == LI_NUMTYPE)) {
        extrn li_sub, li_free, printf;
        
        /* li1 >= li2 <=> li1 - li2 >= 0 (signcheck) */
        auto diff; diff = li_sub(li1, li2);
        auto sgn; sgn = diff[LI_NUM_SIGN];
        auto zro; zro = li_iszero(diff);

        li_free(diff);
        return ((sgn == 0) | zro);
    }
    return (0);
}
li_lt(li1, li2) {
    extrn printf;
    /* TODO: Replace this with a proper bignum system */
    if ((li1[0] == LI_NUMTYPE) & (li2[0] == LI_NUMTYPE)) {
        extrn li_get;
        auto get;
        get = !li_get(li1, li2);
        return (get);
    }
    return (0);
}
li_let(li1, li2) {
    if ((li1[0] == LI_NUMTYPE) & (li1[0] == LI_NUMTYPE)) {
        extrn li_sub, li_free;
        
        /* li1 <= li2 <=> li1 - li2 <= 0 (signcheck) */
        auto diff; diff = li_sub(li1, li2);
        auto sgn; sgn = diff[LI_NUM_SIGN];
        auto zro; zro = li_iszero(diff);
        li_free(diff);
        return ((sgn != 0) | zro);
    }
    return (0);
}
li_eq(li1, li2) {
    if ((li1[0] == LI_NUMTYPE) & (li1[0] == LI_NUMTYPE)) {
        auto i, max;
        if (li1[LI_NUM_SIGN] != li2[LI_NUM_SIGN]) return (0);
        max = li1[LI_NUM_LENGTH];
        if (li2[LI_NUM_LENGTH] > max) max = li2[LI_NUM_LENGTH];

        i = 0; while (i < max) {
            auto w1, w2; w1 = 0; w2 = 0;
            if (i < li1[LI_NUM_LENGTH]) w1 = li1[LI_NUM_DATASTART + i];
            if (i < li2[LI_NUM_LENGTH]) w2 = li2[LI_NUM_DATASTART + i];

            /* nah, they're not equal */
            if (w1 != w2) return (0);
            i++;
        }
        return (1);
    }
    return (0);
}
li_neg(li) {
    if (li[0] == LI_NUMTYPE){
        li = li_copy(li);
        /* TODO: Handle special cases like 0 */
        li[LI_NUM_SIGN] = !li[LI_NUM_SIGN];
        return (li);
    }
    return (li_new(0));
}
li_free(li) {
    extrn free;
    free(li);
}

/* Stack implementation */
stack_index 0;
stack_capacity 0;
stack_zone 0;
stk_init() {
    extrn malloc;
    stack_index = 0;
    stack_capacity = 8;
    stack_zone = malloc(li_ctsize() * stack_capacity);
}
stk_push(li) {
    extrn realloc;
    if (stack_index >= stack_capacity) {
        /* TODO: Curb the growth factor! */
        stack_capacity <<= 1;
        stack_zone = realloc(stack_zone, stack_capacity * li_ctsize());
    }
    stack_zone[stack_index++] = li;
}
stk_pop() {
    auto li;
    if (stack_index == 0) {
        extrn printf;
        printf("?! no more values ?!\n");
        return (li_new(0));
    }

    li = stack_zone[--stack_index];
    return (li);
}
stk_clear() {
    while (--stack_index >= 0) {
        li_free(stack_zone[stack_index]);
    }
    stack_index = 0;
}
stk_destroy() {
    extrn free;
    free(stack_zone);
}
stk_show() {
    auto i;
    extrn printf;
    extrn dc_obase;

    i = stack_index - 1; while (i >= 0) {
        li_show(stack_zone[i--], dc_obase);
        printf("\n");
    }
}

/* RPN operations */
rpn_add() {
    auto li_b, li_a, sum;

    li_b = stk_pop();
    li_a = stk_pop();
    sum = li_add(li_a, li_b);
    stk_push(sum);
    li_free(li_a);
    li_free(li_b);
}
rpn_sub() {
    auto li_b, li_a, sum;

    li_b = stk_pop();
    li_a = stk_pop();
    sum = li_sub(li_a, li_b);
    stk_push(sum);
    li_free(li_a);
    li_free(li_b);
}
rpn_mul() {
    auto li_b, li_a, sum;

    li_b = stk_pop();
    li_a = stk_pop();
    sum = li_mul(li_a, li_b);
    stk_push(sum);
    li_free(li_a);
    li_free(li_b);
}
rpn_exp() {
    auto li_b, li_a, sum;
    extrn li_exp;

    li_b = stk_pop();
    li_a = stk_pop();
    sum = li_exp(li_a, li_b);
    stk_push(sum);
    li_free(li_a);
    li_free(li_b);
}
rpn_mod() {
    auto li_b, li_a, div, divmul, res;

    li_b = stk_pop();
    li_a = stk_pop();

    div = li_div(li_a, li_b);
    divmul = li_mul(div, li_b);
    res = li_sub(li_a, divmul);
    li_free(div);
    li_free(divmul);
    stk_push(res);
    li_free(li_a);
    li_free(li_b);
}
rpn_div() {
    auto li_b, li_a, sum;
    extrn printf;

    li_b = stk_pop();
    li_a = stk_pop();
    sum = li_div(li_a, li_b);
    stk_push(sum);
    li_free(li_a);
    li_free(li_b);
}
rpn_seti() {
    auto li_a;
    extrn dc_ibase;

    li_a = stk_pop();
    dc_ibase = li_ton(li_a);
    li_free(li_a);
}
rpn_seto() {
    auto li_a;
    extrn dc_obase;

    li_a = stk_pop();
    dc_obase = li_ton(li_a);
    li_free(li_a);
}
rpn_swap() {
    auto li_b, li_a;

    li_a = stk_pop();
    li_b = stk_pop();

    stk_push(li_a);
    stk_push(li_b);
}
rpn_get(r) {
    auto li_b, li_a, sum;
    extrn readchar, execute;

    li_a = stk_pop();
    li_b = stk_pop();

    if (li_get(li_a, li_b)) {
        extrn dc_registers;
        auto reg; reg = dc_registers[r];
        if (li_type(reg) == LI_STRTYPE) execute(&reg[1]);
        else                            stk_push(li_copy(reg));
    }

    li_free(li_a);
    li_free(li_b);
}
rpn_gt(r) {
    auto li_b, li_a, sum;
    extrn readchar, execute;

    li_a = stk_pop();
    li_b = stk_pop();

    if (li_gt(li_a, li_b)) {
        extrn dc_registers;
        auto reg; reg = dc_registers[r];
        if (li_type(reg) == LI_STRTYPE) execute(&reg[1]);
        else                            stk_push(li_copy(reg));
    }

    li_free(li_a);
    li_free(li_b);
}
rpn_neq(r) {
    auto li_b, li_a, sum;
    extrn readchar, execute;

    li_a = stk_pop();
    li_b = stk_pop();
    if (!li_eq(li_a, li_b)) {
        extrn dc_registers;
        auto reg; reg = dc_registers[r];
        if (li_type(reg) == LI_STRTYPE) execute(&reg[1]);
        else                            stk_push(li_copy(reg));
    }

    li_free(li_a);
    li_free(li_b);
}
rpn_eq(r) {
    auto li_b, li_a, sum;
    extrn readchar, execute;

    li_a = stk_pop();
    li_b = stk_pop();

    if (li_eq(li_a, li_b)) {
        extrn dc_registers;
        auto reg; reg = dc_registers[r];
        if (li_type(reg) == LI_STRTYPE) execute(&reg[1]);
        else                            stk_push(li_copy(reg));
    }

    li_free(li_a);
    li_free(li_b);
}
rpn_let(r) {
    auto li_b, li_a, sum;
    extrn readchar, execute;

    li_a = stk_pop();
    li_b = stk_pop();

    if (li_let(li_a, li_b)) {
        extrn dc_registers;
        auto reg; reg = dc_registers[r];
        if (li_type(reg) == LI_STRTYPE) execute(&reg[1]);
        else                            stk_push(li_copy(reg));
    }

    li_free(li_a);
    li_free(li_b);
}
rpn_lt(r) {
    auto li_b, li_a, sum;
    extrn readchar, execute;

    li_a = stk_pop();
    li_b = stk_pop();

    if (li_lt(li_a, li_b)) {
        extrn dc_registers;
        auto reg; reg = dc_registers[r];

        // TODO
        if (li_type(reg) == LI_STRTYPE) execute(&reg[1]);
        else                            stk_push(li_copy(reg));
    }

    li_free(li_a);
    li_free(li_b);
}

rpn_showstr(li) {
    auto charsize; charsize = li_newlen(8);
    auto a, c1, c;
    li_setb2(charsize, 8, 1);
    a = li_div(li, charsize);
    c1 = li_mod(li, charsize);

    if (!li_iszero(a))
        rpn_showstr(a);
    c = li_ton(c1);
    printf("%c", c);

    li_free(a);
    li_free(c1);
    li_free(charsize);
}
rpn_popshow() {
    auto li_a, charsize;
    extrn printf;
    extrn dc_obase;

    li_a = stk_pop();
    if (li_type(li_a) != LI_STRTYPE)
        rpn_showstr(li_a);
    else
        printf("%s", &li_a[1]);
    li_free(li_a);
}
rpn_show(end) {
    auto li_a;
    extrn printf;
    extrn dc_obase;

    li_a = stk_pop();
    li_show(li_a, dc_obase);
    printf("%s", end);
    stk_push(li_a);
}
rpn_pushlen() {
    auto li_a;
    extrn printf;
    extrn dc_obase;

    li_a = stk_pop();
    if (li_type(li_a) == LI_STRTYPE) {
        extrn strlen;
        auto len; len = strlen(&li_a[1]);
        stk_push(li_new(len));
    } else {
        auto len; len = 0;
        auto ten; ten = li_new(10);
        li_a[LI_NUM_SIGN] = 0;
        while (li_gt(li_a, ten)) {
            auto tmp; tmp = li_div(li_a, ten);
            len++;
            li_free(li_a);
            li_a = tmp;
        }
        stk_push(li_new(len));
    }
    li_free(li_a);
}
rpn_dup() {
    auto li_a;

    li_a = stk_pop();
    stk_push(li_a);
    stk_push(li_copy(li_a));
}

/* TODO: Allow reading off a string (for macros) */
is_alphanum(ch) {
    if (ch >= '0') if (ch <= '9') return (1);
    if (ch >= 'A') if (ch <= 'F') return (1);
    return (0);
}
to_index(ch) {
    if (ch >= '0') if (ch <= '9') return (ch - '0');
    if (ch >= 'A') if (ch <= 'F') return ((ch - 'A') + 10);
    return (0);
}
readchar(inp) {
    extrn getchar, char;
    auto c;
    if (inp == 0) return (getchar());

    c = char(*inp, 0);
    inp[0] += 1;
    return (c);
}

/* Measured in words */
tmpstr[256];

execute(in) {
    extrn readchar, abort, exit;
    extrn dc_ibase;
    auto chr, li, b;
    auto ptr;
    
    ptr = in == 0 ? 0 : &in;
    while ((chr = readchar(ptr)) != 0) {
        if (is_alphanum(chr)) {
            /* It's time to decode the alphanumeric character */
            auto tmp, tmp1, tmpn;
            li = li_new(to_index(chr));
            b = li_new(dc_ibase);
            while (is_alphanum((chr = readchar(ptr)))) {
                tmp = li_mul(li, b);
                tmpn = li_new(to_index(chr));
                tmp1 = li_add(tmp, tmpn);
                li_free(tmp);
                li_free(tmpn);
                li_free(li);
                li = tmp1;
            }
            stk_push(li);
            li_free(b);
        } else if ((chr == '_') | (chr == '-')) {
            /* It's time to decode the alphanumeric character */
            auto tmp, tmp1, tmpn;
            li = li_new(0);
            b = li_new(dc_ibase);
            while (is_alphanum((chr = readchar(ptr)))) {
                tmp = li_mul(li, b);
                tmpn = li_new(to_index(chr));
                tmp1 = li_add(tmp, tmpn);
                li_free(tmp);
                li_free(tmpn);
                li_free(li);
                li = tmp1;
            }
            tmp = li_neg(li);
            li_free(li);
            stk_push(tmp);
            li_free(b);
        }
        /* Interpret commands */
             if (chr == '+') rpn_add();
        else if (chr == '-') rpn_sub();
        else if (chr == '*') rpn_mul();
        else if (chr == '^') rpn_exp();
        else if (chr == '/') rpn_div();
        else if (chr == '%') rpn_mod();
        else if (chr == 'd') rpn_dup();
        else if (chr == 'r') rpn_swap();
        else if (chr == 'f') stk_show();
        else if (chr == 'c') stk_clear();
        else if (chr == 'p') rpn_show("\n");
        else if (chr == 'P') rpn_popshow();
        else if (chr == 'n') { rpn_show(""); li_free(stk_pop()); }
        else if (chr == 'k') { extrn dc_sf; li_free(dc_sf); dc_sf = stk_pop(); }            /* TODO: Skalefactors */
        else if (chr == 'K') { extrn dc_sf; stk_push(li_copy(dc_sf)); }
        else if (chr == 'q') { return(1); }
        else if (chr == 'Z') rpn_pushlen();
        else if (chr == 'i') rpn_seti();
        else if (chr == 'o') rpn_seto();
        else if (chr == 'v') rpn_seto();
        else if (chr == 'z') stk_push(li_new(stack_index));
        else if (chr == '>') rpn_gt(readchar(ptr));
        else if (chr == '<') rpn_lt(readchar(ptr));
        else if (chr == '=') rpn_eq(readchar(ptr));
        else if (chr == '#') {
            while (((chr = readchar(ptr)) != '\n') & (chr != 0)) {}
        }
        else if (chr == '!') {
            chr = readchar(ptr);
            if (chr == '>') rpn_let(readchar(ptr));
            else if (chr == '<') rpn_get(readchar(ptr));
            else if (chr == '=') rpn_neq(readchar(ptr));
            else {
                extrn printf, abort;
                printf("?!\n");
                abort();
            }
        }
        else if (chr == 's') {
            extrn dc_registers;

            chr = readchar(ptr) & 0xFF;
            li_free(dc_registers[chr]);
            dc_registers[chr] = stk_pop();
        }
        else if (chr == 'l') {
            extrn dc_registers;

            chr = readchar(ptr) & 0xFF;
            stk_push(li_copy(dc_registers[chr]));
        } else if (chr == 'x') {
            extrn dc_registers;
            auto li, type;

            li = stk_pop();
            type = li_type(li);
            if (type == LI_NUMTYPE) {
                stk_push(li);
            } else {
                /* Execute that string */
                if (execute(&li[1]) == 1)
                    return(0);
            }
        }
        else if (chr == '[') {
            auto level;
            auto str;
            auto i;
            extrn printf, abort, malloc, realloc;
            extrn lchar;

            level = 1; while (level > 0) {
                chr = readchar(ptr);
                if (chr == 0) {
                    printf("?! BAD BRACES ?!\n");
                    abort();
                }
                if (chr == '\\') {
                    chr = readchar(ptr);
                    lchar(tmpstr, i++, chr);
                    lchar(tmpstr, i+0, 0  );
                } else {
                    if (chr == '[') level++;
                    if (chr == ']') level--;
                    if (level > 0) {
                        lchar(tmpstr, i++, chr);
                        lchar(tmpstr, i+0, 0  );
                    }
                }
            }
            stk_push(li_str(tmpstr));
        }
        else if ((chr == ' ') | (chr == '\n') | (chr == '\r')) {}
        else if (chr >= 128) exit(1);
        else if (chr == 0) return (0);
        else {
            extrn printf;
            printf("?! '%d %llu' ?!\n", chr, chr);
        }

    }
    return(0);
}

dc_ibase 10;
dc_obase 10;

/* TODO: Represent each register as a stack of values for S and L operations */
dc_registers[256];

/* TODO: Represents the _power of 10_ that numbers are scaled by */
dc_implicit_scale 0;
dc_sf;

main() {
    extrn printf;
    auto i;
    WORD_LENGTH = &0[1];
    BWORD_LENGTH = WORD_LENGTH * 8;
    MAXWORD = (1<<((BWORD_LENGTH)-1))-1;
    i = BWORD_LENGTH>>1;
    MAXWORD_ROOT = (1<<(i-1))-1;
    

    dc_sf = li_new(0);
    i = 0; while (i < 256)
        dc_registers[i++] = li_new(0);

    stk_init();
    execute(0);
    stk_destroy();
}
