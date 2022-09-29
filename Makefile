all:
	@for n in $$(seq -f "%02g" 1 25); do \
		echo "$$n\n--------"; \
		\time scheme --script $$n/day$$n.scm; \
		echo "";\
	done

scm1:
	@scheme --script 01/day01.scm
scm2:
	@scheme --script 02/day02.scm
scm3:
	@scheme --script 03/day03.scm
scm4:
	@scheme --script 04/day04.scm
scm5:
	@scheme --script 05/day05.scm
scm6:
	@scheme --script 06/day06.scm
scm7:
	@scheme --script 07/day07.scm
scm8:
	@scheme --script 08/day08.scm
scm9:
	@scheme --script 09/day09.scm
scm10:
	@scheme --script 10/day10.scm
scm11:
	@scheme --script 11/day11.scm
scm12:
	@scheme --script 12/day12.scm
scm13:
	@scheme --script 13/day13.scm
scm14:
	@scheme --script 14/day14.scm
scm15:
	@scheme --script 15/day15.scm
scm16:
	@scheme --script 16/day16.scm
scm17:
	@scheme --script 17/day17.scm
scm18:
	@scheme --script 18/day18.scm
