package com.polus.fibicomp.questionnaire.dto;

import java.util.HashSet;
import java.util.Set;

public class DeleteDto {

	private Set<Integer> condition = new HashSet<Integer>();

	private Set<Integer> option = new HashSet<Integer>();

	private Set<Integer> question = new HashSet<Integer>();

	public Set<Integer> getCondition() {
		return condition;
	}

	public void setCondition(Set<Integer> condition) {
		this.condition = condition;
	}

	public Set<Integer> getOption() {
		return option;
	}

	public void setOption(Set<Integer> option) {
		this.option = option;
	}

	public Set<Integer> getQuestion() {
		return question;
	}

	public void setQuestion(Set<Integer> question) {
		this.question = question;
	}

}
