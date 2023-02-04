void nop() {
        // do nothing
}

int main() {
        int x = 5;
        if (false) x = 7; else x = 7;
        printInt(x);
        return 0;
}
