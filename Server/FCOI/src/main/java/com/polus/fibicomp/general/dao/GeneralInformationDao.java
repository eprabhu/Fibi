package com.polus.fibicomp.general.dao;

import java.util.List;

import com.polus.fibicomp.general.pojo.DynamicModuleConfig;
import com.polus.fibicomp.general.pojo.DynamicSectionConfig;
import com.polus.fibicomp.general.pojo.HelpText;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.vo.CommonVO;

public interface GeneralInformationDao {

	public Sponsor fetchSponsorData(String sponsorCode);

	public Integer insertSponsorData(Sponsor sponsor);

	public void updateSponsorData(Sponsor sponsor);

	public void deleteSponsorData(Sponsor sponsor);

	public List<SponsorType> fetchSponsorTypeLookup();

	public String nextSponsorCode();

	public String syncPersonRole();

	public CommonVO getAllSponsors(CommonVO vo);

	public boolean evaluateRule(Integer moduleCode, Integer subModuleCode, String moduleItemKey, Integer ruleId, String logginPersonId, String updateUser, String subModuleItemKey);

	public HelpText gethelpText(Integer moduleCode, Integer sectionTypeCode);

	public List<HelpText> gethelpTextBasedOnHelpTextId(Integer parentHelpTextId);
	
	public List<DynamicSectionConfig> getSectionConfiguration(String moduleCode);

	public List<DynamicModuleConfig> getModuleConfiguration();

}
