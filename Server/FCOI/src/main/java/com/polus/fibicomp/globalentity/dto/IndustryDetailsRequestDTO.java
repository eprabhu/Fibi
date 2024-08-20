package com.polus.fibicomp.globalentity.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class IndustryDetailsRequestDTO {

	private Integer entityId;
	private List<Integer> entityIndustryCatIds;
	private Integer entityIndustryCatId;
	private String entityIndustryClassId;
	private String primaryCatId;

}
