int main () {
	int i = 1;
	printInt(i);  //1
	true || i++ != 45;
	printInt(i); //1
	false || i++ >= 0;
	printInt(i); //2
	true && i++ < 0;
	printInt(i); //3
	false && i++ > 0;
	printInt(i);
	int j = 0;
      // Lab 3 only comment:
      // Initialization of j would not be needed as the access of j
      // is in dead code, if compiled correctly.
      // However, the JVM bytecode verifier is picky and will complain.
	if (34 < 6 && j < 0) {
		printInt(i);
	} else {
		printInt (42);
	}

	return 0;
}
