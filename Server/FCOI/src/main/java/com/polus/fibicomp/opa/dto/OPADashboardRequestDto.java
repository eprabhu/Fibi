package com.polus.fibicomp.opa.dto;

import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.Pattern;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter
@Setter
public class OPADashboardRequestDto {

    private Integer id;
    private String tabType;
    private String filterType;
    private Integer pageNumber;
    private Integer currentPage;
    private Map<@Pattern(regexp="^$|[a-zA-Z\\.]+$", message="Sort key must not include special characters.")
            String, @Pattern(regexp="^$|[a-zA-Z]+$", message="Sort value must not include special characters.")
            String> sort = new HashMap<>();
    private String unitNumber;
    private String submissionTimestamp;
    private List<String> dispositionStatusCodes;
    private List<String> opaDisclosureStatusCodes;
}
