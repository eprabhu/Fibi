import {Component, OnDestroy, OnInit} from '@angular/core';
import {UserDashboardService} from "./user-dashboard.service";
import {subscriptionHandler} from "../../../../fibi/src/app/common/utilities/subscription-handler";
import { openModal } from '../../../../fibi/src/app/common/utilities/custom-utilities';
import {Router} from "@angular/router";

declare var $: any;
@Component({
  selector: 'app-user-dashboard',
  templateUrl: './user-dashboard.component.html',
  styleUrls: ['./user-dashboard.component.scss'],
  providers: [UserDashboardService]
})
export class UserDashboardComponent implements OnInit, OnDestroy{

  hasFCOI: any;
  hasActiveFCOI = false;
  isModalOpen= false;
  reviseObject: any = { reviseComment: null, disclosureId: null };
  isReadMore = false;
  headerInfoText = `University COI Policy: To identify and evaluate Conflicts of Interest or Commitment, as well as the
                    perception of such conflicts, University Community Members must disclose any Significant Financial
                    Interests, outside activities, and Financial Interests that may lead to or appear to lead to
                    Conflicts of Interest to the appropriate University officials beforehand. The primary disclosure is
                    reviewed by COI Administrators, followed by a secondary review by the Office of Conflict of Interest
                    and Commitment Management. These officials will analyze the disclosures to determine whether a
                    Conflict of Interest exists and what measures or limitations should be implemented to manage,
                    diminish, or eliminate the Conflict of Interest.`;
  $subscriptions = [];

  constructor(public service: UserDashboardService, private _router: Router) {
  }

  ngOnInit(): void {
    this.getActiveDisclosure();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  getActiveDisclosure() {
    this.$subscriptions.push(this.service.getActiveDisclosure().subscribe((res: any) => {
      this.service.activeDisclosures = res.coiDisclosures || [];
      this.updateFCOIStatuses();
    }))
  }

  updateFCOIStatuses() {
    this.hasFCOI = this.service.activeDisclosures.find(disclosure =>
        disclosure.coiDisclosureCategoryType.disclosureCategoryTypeCode == '1'
    );
    this.hasActiveFCOI = this.hasFCOI ? this.hasFCOI.coiDisclosureStatus.disclosureStatusCode == 3 : false;
  }

  clearModal(): void {
    this.reviseObject.reviseComment = null;
    document.getElementById('triggerReviseModal').click();
  }

  openReviseModal() {
    this.reviseObject = { reviseComment: null, disclosureId: null };
    this.reviseObject.disclosureId = this.hasFCOI.disclosureId;
    this.reviseObject.reviseComment = '';
    document.getElementById('triggerReviseModal').click();
  }

  reviseDisclosure() {
    document.getElementById('triggerReviseModal').click();
    this.$subscriptions.push(this.service.reviseDisclosure(this.reviseObject)
        .subscribe((data: any) => {
              // this.reviseDisclosureObject = data;
              // this.clearModal();
              // this._commonService.showToast(HTTP_SUCCESS_STATUS, 'New version of disclosure created.');
              this._router.navigate(['/coi/disclosure/summary'], {
                queryParams: {
                  disclosureId: data.coiDisclosure.disclosureId
                }
              });
            },
            err => {
              // this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in creating new version. Please try again.');
            }));
  }

}
