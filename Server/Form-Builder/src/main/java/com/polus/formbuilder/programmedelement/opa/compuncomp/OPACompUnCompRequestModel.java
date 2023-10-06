package com.polus.formbuilder.programmedelement.opa.compuncomp;

import com.polus.formbuilder.programmedelement.ProgrammedElementModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OPACompUnCompRequestModel implements ProgrammedElementModel{

	private int id;
    private int opaId;
    private int opaNumber;
    private int personEntityId;
    private String entityNumber;
    private String natureOfRelationship;
    private String isCompOrUncomp;
    private int numberOfDays;
}
