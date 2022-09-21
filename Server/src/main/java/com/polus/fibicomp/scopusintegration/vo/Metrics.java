package com.polus.fibicomp.scopusintegration.vo;

import java.util.Map;

public class Metrics {

	private String metricType;

	private Map<Integer, Integer> valueByYear;

	public String getMetricType() {
		return metricType;
	}

	public void setMetricType(String metricType) {
		this.metricType = metricType;
	}

	public Map<Integer, Integer> getValueByYear() {
		return valueByYear;
	}

	public void setValueByYear(Map<Integer, Integer> valueByYear) {
		this.valueByYear = valueByYear;
	}
}
