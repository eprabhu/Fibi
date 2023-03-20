import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { fileDownloader } from '../../common/utilities/custom-utilities';
import { ExpandedWidgetsService } from '../expanded-widgets.service';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../app-constants';

@Component({
  selector: 'app-institute-proposal-by-sponsor-expanded-view',
  templateUrl: './institute-proposal-by-sponsor-expanded-view.component.html',
  styleUrls: ['./institute-proposal-by-sponsor-expanded-view.component.css']
})
export class InstituteProposalBySponsorExpandedViewComponent implements OnInit {

  heading = null;
  isDesc: boolean;
  column: number;
  direction: number;
  tabName: any;
  currentStatus: string;
  sponsorNumber: any;
  $subscriptions: Subscription[] = [];
  detailedViewOfWidget: any = [];

  constructor(private _route: ActivatedRoute, private _expandedWidgetsService: ExpandedWidgetsService, private _router: Router,
    private _commonService: CommonService) { }

  ngOnInit() {
    this.tabName = this._route.snapshot.queryParamMap.get('tabName');
    this.setStatus();
    this.getDetailedData();
    this.heading = this.currentStatus + ' Institute proposal by ' + this._route.snapshot.queryParamMap.get('sponsorName');
  }

  exportAsTypeDoc(docType) {
    const REQ_BODY = {
      documentHeading: this.heading,
      exportType: docType === 'excel' ? 'xlsx' : docType === 'pdf' ? 'pdf' : '',
      researchSummaryIndex: this.tabName,
      sponsorCodes: [this._route.snapshot.queryParamMap.get('sponsorNumber')]
    }
    this.$subscriptions.push(this._expandedWidgetsService.exportResearchSummaryData(REQ_BODY).subscribe((res: any) => {
      fileDownloader(res.body, this.heading.toLowerCase(), REQ_BODY.exportType);
    }, error => {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Downloading the attachment failed. Please try again.');
    }));
  }

  sortBy(property) {
    this.column = property;
    this.direction = this.isDesc ? 1 : -1;
  }

  openInstituteProposal(proposalId) {
    this._router.navigate(['/fibi/instituteproposal/overview'], {
      queryParams: {
        instituteProposalId: proposalId
      }
    });
  }

  setStatus() {
    switch (this.tabName) {
      case 'PENDING_INSTITUTE_PROPOSAL_SPONSOR': this.currentStatus = 'Pending';
        break;
      case 'NOT_FUNDED_INSTITUTE_PROPOSAL_SPONSOR': this.currentStatus = 'Not Funded';
        break;
      case 'FUNDED_INSTITUTE_PROPOSAL_SPONSOR': this.currentStatus = 'Funded';
        break;
      case 'WITHDRAWN_INSTITUTE_PROPOSAL_SPONSOR': this.currentStatus = 'Withdrawn';
        break;
      default: null;
    }
  }

  getDetailedData() {
    const REQ_BODY = {
      tabName: this.tabName,
      sponsorCodes: [this._route.snapshot.queryParamMap.get('sponsorNumber')]
    };
    this.$subscriptions.push(this._expandedWidgetsService.getDetailedViewOfWidget(REQ_BODY).subscribe((data: any) => {
      this.detailedViewOfWidget = data.widgetDatas || [];
    }, error => {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Something Went wrong!');
    }));
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
}
