package com.polus.formbuilder.formconfig.v1.model;

import java.util.List;

import com.polus.formbuilder.entity.FormBuilderComponentTypeEntity;
import com.polus.formbuilder.entity.FormBuilderProgElementEntity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class FormDataResponseModel {

	 FormHeaderModel formHeader;
	 
	 List<FormBuilderProgElementEntity> lookupProgramElements;
	 
	 List<FormBuilderComponentTypeEntity> lookupSectionComponentType;
}
