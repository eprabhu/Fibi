package com.polus.fibicomp.globalentity.dashboard.dto;

import java.util.Map;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class EntityDashboardDTO {

	private Integer pageNumber;

	private Integer currentPage;

	private String personId;

	private Boolean isDownload;

	private Map<String, Object> entityDashboardData;

}
