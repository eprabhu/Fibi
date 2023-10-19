package com.polus.formbuilder.model;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ApplicableFormResponse {

	private List<Integer> applicableFormsBuilderIds;
	
	private Integer formsBuilderId;
	
}
