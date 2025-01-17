package com.polus.formbuilder.programmedelement.opa.outsidefinancialinterest;

import java.util.List;

import com.polus.formbuilder.programmedelement.ProgrammedElementModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OPAOutsideFinancialInterestResponseModel implements ProgrammedElementModel{

		List<OPAOutsideFinancialInterestResponseDTO> data;

}
