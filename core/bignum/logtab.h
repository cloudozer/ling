// Copyright (c) 2013-2014 Cloudozer LLP. All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// 
// * Redistributions in any form must be accompanied by information on how to
// obtain complete source code for the LING software and any accompanying
// software that uses the LING software. The source code must either be included
// in the distribution or be available for no more than the cost of distribution
// plus a nominal fee, and must be freely redistributable under reasonable
// conditions.  For an executable file, complete source code means the source
// code for all modules it contains. It does not include source code for modules
// or files that typically accompany the major components of the operating
// system on which the executable file runs.
// 
// THIS SOFTWARE IS PROVIDED BY CLOUDOZER LLP ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT, ARE
// DISCLAIMED. IN NO EVENT SHALL CLOUDOZER LLP BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

const double s_logv_2[] = {
   0.000000000, 0.000000000, 1.000000000, 0.630929754, 	/*  0  1  2  3 */
   0.500000000, 0.430676558, 0.386852807, 0.356207187, 	/*  4  5  6  7 */
   0.333333333, 0.315464877, 0.301029996, 0.289064826, 	/*  8  9 10 11 */
   0.278942946, 0.270238154, 0.262649535, 0.255958025, 	/* 12 13 14 15 */
   0.250000000, 0.244650542, 0.239812467, 0.235408913, 	/* 16 17 18 19 */
   0.231378213, 0.227670249, 0.224243824, 0.221064729, 	/* 20 21 22 23 */
   0.218104292, 0.215338279, 0.212746054, 0.210309918, 	/* 24 25 26 27 */
   0.208014598, 0.205846832, 0.203795047, 0.201849087, 	/* 28 29 30 31 */
   0.200000000, 0.198239863, 0.196561632, 0.194959022, 	/* 32 33 34 35 */
   0.193426404, 0.191958720, 0.190551412, 0.189200360, 	/* 36 37 38 39 */
   0.187901825, 0.186652411, 0.185449023, 0.184288833, 	/* 40 41 42 43 */
   0.183169251, 0.182087900, 0.181042597, 0.180031327, 	/* 44 45 46 47 */
   0.179052232, 0.178103594, 0.177183820, 0.176291434, 	/* 48 49 50 51 */
   0.175425064, 0.174583430, 0.173765343, 0.172969690, 	/* 52 53 54 55 */
   0.172195434, 0.171441601, 0.170707280, 0.169991616, 	/* 56 57 58 59 */
   0.169293808, 0.168613099, 0.167948779, 0.167300179, 	/* 60 61 62 63 */
   0.166666667
};

