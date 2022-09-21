package com.polus.fibicomp.report.vo;

import java.util.List;

public class Report {
	
	private String name;
	
	private String description;
	
	private String group;
	
	private List<Field> input;
	
	private List<Field> output;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getGroup() {
		return group;
	}

	public void setGroup(String group) {
		this.group = group;
	}

	public List<Field> getInput() {
		return input;
	}

	public void setInput(List<Field> input) {
		this.input = input;
	}

	public List<Field> getOutput() {
		return output;
	}

	public void setOutput(List<Field> output) {
		this.output = output;
	}

}
