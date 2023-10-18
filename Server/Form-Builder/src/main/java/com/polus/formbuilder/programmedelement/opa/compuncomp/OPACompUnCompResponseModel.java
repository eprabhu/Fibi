package com.polus.formbuilder.programmedelement.opa.compuncomp;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import com.polus.formbuilder.programmedelement.ProgrammedElementModel;

import jakarta.persistence.Column;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OPACompUnCompResponseModel implements ProgrammedElementModel{

		List<OPACompUnCompResponseDTO> data;

}
