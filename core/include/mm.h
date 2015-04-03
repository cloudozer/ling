#pragma once

void mm_init(void);
void *mm_alloc_pages(int nr_pages);
int mm_alloc_left(void);
void *mm_alloc_tmp(void);

//EOF
