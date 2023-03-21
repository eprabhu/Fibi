package com.polus.fibicomp.manpowerintegration.scheduler;

import java.text.ParseException;
import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;
import com.polus.fibicomp.manpowerintegration.vo.ManpowerIntegrationVO;

@Service
public interface ManpowerIntegrationSchedulerService {

	public void workdayManpowerInterface(Award award, List<WorkdayManpowerInterface> workdayManpowerInterfaces, StringBuilder superiorSupOrg, Boolean isRetrigger) throws ParseException, Exception;

	public void checkForManpowerIntegration(Award award, Award activeAward, AwardPerson newAwardPIPerson, AwardPerson activeAwardPIPerson, String activeAwardSuperiorSupOrg);

	public void getAllJobProfiles();

	void closePosition();

	void getAllManpowerDetails();

	void getCitizenshipNationalityDetails();

	public void assignCostAllocation();

	void startCostingAllocationReconciliation();

	public void sendMailToCitsAfterCutOffDate(List<WorkdayManpowerInterface> allocationCutOffInterfaces);
	
	public void getManpowerLogMail();

	public void startWorkdayLongLeave();

	public void startWorkdayTerminations();

	public void encryptAllMigratedCitizenshipNationality();

	public void getJobProfileChanges();

	public String retriggerAwardWorkdayPrerequisite(ManpowerIntegrationVO vo);

	public String retriggerWorkdayApi(ManpowerIntegrationVO vo);

	public void assignCostAllocationDetails(List<WorkdayManpowerInterface> interfaces);

	public void threadForFetchAndIntegrateManpower(String awardNumber);

	public void integrateManpower(String awardNumber);

	public String retriggerWorkdayClosePosition(ManpowerIntegrationVO vo);

}
