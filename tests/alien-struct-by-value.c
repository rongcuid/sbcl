struct large_align_8 {
    long long m0, m4, m8, m12;
    long long m1, m5, m9, m13;
    long long m2, m6, m10, m14;
    long long m3, m7, m11, m15;
};
#define large_align_8_test(member) \
  long long large_align_8_test_##member(struct large_align_8 m) { \
    return m.member; \
  }
large_align_8_test(m0);
large_align_8_test(m1);
large_align_8_test(m2);
large_align_8_test(m3);
large_align_8_test(m4);
large_align_8_test(m5);
large_align_8_test(m6);
large_align_8_test(m7);
large_align_8_test(m8);
large_align_8_test(m9);
large_align_8_test(m10);
large_align_8_test(m11);
large_align_8_test(m12);
large_align_8_test(m13);
large_align_8_test(m14);
large_align_8_test(m15);

void large_align_8_test_mutate(struct large_align_8 m) {
  m.m0++;
  m.m1++;
  m.m2++;
  m.m3++;
  m.m4++;
  m.m5++;
  m.m6++;
  m.m7++;
  m.m8++;
  m.m9++;
  m.m10++;
  m.m11++;
  m.m12++;
  m.m13++;
  m.m14++;
  m.m15++;
}
