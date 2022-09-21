import { Injectable } from '@angular/core';
import { BehaviorSubject, Subject } from 'rxjs';
import { WebSocketService } from '../../common/services/web-socket.service';
import { CommonService } from '../../common/services/common.service';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../common/utilities/date-utilities';

@Injectable()

export class BudgetDataService {

  public proposalBudgetData = new BehaviorSubject<any>(null);
  public personData = new BehaviorSubject<any>(null);
  isProposalBudgetPrintTrigger = new Subject();
  isPeriodOperationsProposalTrigger = new Subject();
  proposalStatusChange = new BehaviorSubject<Boolean>(false);
  isBudgetViewMode = false;
  departmentLevelRightsForProposal: any = {};
  BudgetTab = 'PERIODSTOTAL';
  budgetData: any = {};
  isBudgetDatesFilled = true;
  previousFinalBudgetId = null;
  budgetDataChanged = false;
  isBudgetOverviewChanged = false;
  currentPeriodNumber = 1;
  activityTypeCode = null;
  grantTypeCode = null;
  $budgetHelpText = new BehaviorSubject<any>(null);

  constructor(public _commonService: CommonService , private websocket: WebSocketService) { }

  setProposalBudgetData(budgetData) {
    this.budgetData = budgetData;
    this.convertDateObject();
    this.proposalBudgetData.next(this.budgetData);
  }

  updateBudgetOnProposalStatusChange(change: Boolean) {
    this.proposalStatusChange.next(change);
  }

  /**
  * Converting budget start date and date date from time stamp to date object.
  */
  convertDateObject() {
    if (this.budgetData && this.budgetData.budgetHeader) {
      this.budgetData.budgetHeader.startDate = getDateObjectFromTimeStamp(this.budgetData.budgetHeader.startDate);
      this.budgetData.budgetHeader.endDate = getDateObjectFromTimeStamp(this.budgetData.budgetHeader.endDate);
      this.budgetData.budgetHeader.budgetPeriods.map(period => {
        period.startDate = getDateObjectFromTimeStamp(period.startDate);
        period.endDate = getDateObjectFromTimeStamp(period.endDate);
        this.convertPersonDates(period.budgetDetails);
      });
    }
  }

  convertPersonDates(lineItems) {
    lineItems.forEach(item => {
      if (item.personsDetails) {
        item.personsDetails.map(person => {
          person.startDate = getDateObjectFromTimeStamp(person.startDate);
          person.endDate = getDateObjectFromTimeStamp(person.endDate);
        });
      }
    });
  }

  checkBudgetDatesFilled(periods = this.budgetData.budgetHeader.budgetPeriods) {
    this.isBudgetDatesFilled = periods.find(period => period.startDate == null || period.endDate == null) ? false : true;
    return this.isBudgetDatesFilled;
  }

  setBudgetPersonData(personData) {
    this.personData.next(personData);
  }

  setDateFormatFromWithoutTimeStamp(budgetData = this.budgetData) {
    budgetData.budgetHeader.startDate = parseDateWithoutTimestamp(budgetData.budgetHeader.startDate);
    budgetData.budgetHeader.endDate = parseDateWithoutTimestamp(budgetData.budgetHeader.endDate);
    budgetData.budgetHeader.budgetPeriods.forEach(element => {
      element.startDate = parseDateWithoutTimestamp(element.startDate);
      element.endDate = parseDateWithoutTimestamp(element.endDate);
      element.budgetDetails.forEach(el => {
        if (el.personsDetails) {
          el.personsDetails.forEach(e => {
            e.startDate = parseDateWithoutTimestamp(e.startDate);
            e.endDate = parseDateWithoutTimestamp(e.endDate);
          });
        }
      });
    });
  }

  /** Method to enable edit/view mode based on logged in user's budget access rights & status of proposal.
  * EDIT mode      : When logged in user has 'MAINTAIN_PROPOSAL_BUDGET' right & Proposal is in
  *                  Draft(1) / In Progress(1) / Returned(3) / Revision Requested(9) / Pending Revision(by GM:20, by GA:24, 22) statuses.
  * APPROVED BUDGET: When logged user have 'DEFINE_APPROVED_BUDGET' right, he can create approved budget
  *                  and it will be editable upto 'Awarded' status for user with that right.
  * Budget Versions submitted by PI is non-editable for both PI/Reviewers when the proposal undergoing
    review process(excluding revision requested/pending revision statuses)
  */
  setBudgetEditMode(mode, proposalStatusCode) {
    if ((mode !== 'view' && this.departmentLevelRightsForProposal.isMaintainProposalBudget) ||
      (mode === 'view' && ([1, 9, 12, 20, 22, 24].includes(proposalStatusCode))
        && this.departmentLevelRightsForProposal.isMaintainProposalBudget)) {
      if (this.budgetData.budgetHeader) {
        this.isBudgetViewMode = this.budgetData.budgetHeader.isApprovedBudget ? true : false;
      } else {
        this.isBudgetViewMode = false; /**to show create new budget button */
      }
    } else if (mode === 'view' && this.departmentLevelRightsForProposal.isDefineApprovedBudget &&
      !([11, 1, 29, 30, 35, 1, 9, 20, 22, 24, 12].includes(proposalStatusCode))) {
      if (this.budgetData.budgetHeader) {
        this.isBudgetViewMode = this.budgetData.budgetHeader.isApprovedBudget ? false : true;
      } else {
        this.isBudgetViewMode = false; /**to show create approved budget button */
      }
    } else {
      this.isBudgetViewMode = true;
    }
    if (!this.isBudgetViewMode) {
      this.isBudgetViewMode =  this.websocket.isModuleLockAvailable === 'NotAvailAble' ?  true : false;
    }
  }

}


