import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { fadeDown } from '../../common/utilities/animations';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { ResearchSummaryWidgetsService } from '../research-summary-widgets.service';

@Component({
  selector: 'app-proposal-submitted-by-leadunit',
  templateUrl: './proposal-submitted-by-leadunit.component.html',
  styleUrls: ['./proposal-submitted-by-leadunit.component.css'],
  animations: [fadeDown]
})
export class ProposalSubmittedByLeadunitComponent implements OnInit, OnDestroy {

  $subscriptions: Subscription[] = [];
  isShowLoader = false;
  proposalByLeadUnitData: any = [];
  proposalByLeadUnitDataHeading: any = [];
  widgetDescription: any;
  totalProposalCount: number;
  currentFY: any;
  column: number;
  direction: number = -1;
  isDesc: boolean;

  constructor(private _researchSummaryWidgetService: ResearchSummaryWidgetsService) { }

  ngOnInit() {
    this.widgetDescription = this._researchSummaryWidgetService.getWidgetDescription(25);
    this.getProposalByLeadUnitData();
  }

  getProposalByLeadUnitData() {
    this.isShowLoader = true;
    this.$subscriptions.push(this._researchSummaryWidgetService.
      getResearchSummaryDatasByWidget({ unitNumber: null, tabName: 'PROPOSAL_SUBMITTED_BY_LEAD_UNIT' })
      .subscribe((data: any) => {
        this.proposalByLeadUnitData = data.widgetDatas || [];
        this.getTotalSubmittedProposal();
        this.currentFY = this._researchSummaryWidgetService.getCurrentFinancialYear();
        // this.proposalByLeadUnitDataHeading = this.proposalByLeadUnitData.splice(0, 1)[0];
        this.isShowLoader = false;
      }, err => { this.isShowLoader = false; }));
  }

  sortBy(property) {
    this.column = property;
    this.direction = this.isDesc ? 1 : -1;
  }

  /**
   * {number} is the column number or the value the sum should be taken
   */
   getTotalSubmittedProposal(): void {
    this.totalProposalCount = this._researchSummaryWidgetService.getSumOfColumn(this.proposalByLeadUnitData, 1);
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

}
