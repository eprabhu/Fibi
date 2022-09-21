package com.polus.fibicomp.evaluation.comparator;

import java.util.Comparator;

import org.apache.commons.lang3.builder.CompareToBuilder;

import com.polus.fibicomp.evaluation.pojo.ReviewComment;

public class EvaluvationCompartorByReviewComment implements Comparator<ReviewComment> {

	@Override
	public int compare(ReviewComment r1, ReviewComment r2) {
		return new CompareToBuilder().append(r2.getUpdateTimestamp(), r1.getUpdateTimestamp()).toComparison();
	}

}
