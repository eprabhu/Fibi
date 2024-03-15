import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { SharedSfiService } from './shared-sfi.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { SfiObject } from '../shared-interface';
import { CoiService } from '../../disclosure/services/coi.service';

@Component({
  selector: 'app-shared-sfi-card',
  templateUrl: './shared-sfi-card.component.html',
  styleUrls: ['./shared-sfi-card.component.scss'],
  providers: [SharedSfiService, CoiService]
})

export class SharedSfiCardComponent implements OnInit, OnDestroy {

  @Input() reqObject: any;
  @Input() isTriggeredFromSlider: any;
  @Input() referredFrom: 'SFI_SUMMARY' | 'SFI_EDIT_AND_DASHBOARD' | 'TRAVEL_DISCLOSURE';
  @Output() viewSlider = new EventEmitter<any>();
  @Output() deleteEvent =  new EventEmitter<any>();
  @Output() activateDeactivateEvent =  new EventEmitter<any>();
  @Output() reviewSlider = new EventEmitter<any>();

  SFIObject = new SfiObject();
  $subscriptions: Subscription[] = [];

  constructor(private _router: Router, private _sharedSFIService: SharedSfiService, private _commonService: CommonService, private _coiService: CoiService) { }

    ngOnInit() {
      this.updateSFIObject();
    }

    ngOnDestroy() {
      subscriptionHandler(this.$subscriptions);
    }

  private updateSFIObject(): void {
    if (this.reqObject) {
      this.SFIObject.isActive = this.reqObject.versionStatus === 'ACTIVE' || this.reqObject.versionStatus === 'ARCHIVE';
      this.SFIObject.entityId =  this.reqObject.personEntityId;
      this.SFIObject.entityNumber = this.reqObject.personEntityNumber;
      this.SFIObject.entityType = this.getEntityDescription();
      this.SFIObject.countryName = this.getCountryName();
      this.SFIObject.involvementEndDate = this.reqObject.involvementEndDate;
      this.SFIObject.involvementStartDate = this.reqObject.involvementStartDate;
      this.SFIObject.validPersonEntityRelTypes = this.groupBy(this.reqObject.validPersonEntityRelTypes, "coiDisclosureType", "description");
      this.SFIObject.entityName = this.getValuesFormCOIEntityObj('entityName');
      this.SFIObject.canDelete = this.reqObject.canDelete;
      this.SFIObject.isFormCompleted = this.reqObject.isFormCompleted;
    }
  }

  private getEntityDescription(): string|null {
    return this.getValuesFormCOIEntityObj('entityType') ? this.getValuesFormCOIEntityObj('entityType').description : null;
  }

  private getCountryName(): string|null {
    return this.getValuesFormCOIEntityObj('country') ? this.getValuesFormCOIEntityObj('country').countryName : null;
  }

  private getValuesFormCOIEntityObj(value): any {
    return this.reqObject.coiEntity ? this.reqObject.coiEntity[value] : null;
  }

  openSfiDetails(entityId: number, mode: string): any {
    this.isTriggeredFromSlider ? this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: entityId, personEntityNumber: this.SFIObject.entityNumber } })
                               : this.viewSlider.emit({flag: true, entityId: entityId});
  }

    modifySfiDetails(entityId: number, mode: string): void {
      this.$subscriptions.push(this._sharedSFIService.modifySfi({ personEntityId: entityId, personEntityNumber: this.SFIObject.entityNumber }).subscribe((res: any) => {
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: res.personEntityId, personEntityNumber: res.personEntityNumber, mode: 'E'} });
      }));
    }

  deleteConfirmation() {
    this.deleteEvent.emit({eId: this.SFIObject.entityId});
  }

  activateDeactivate() {
    this.activateDeactivateEvent.emit(this.reqObject);
  }

  openReviewComment(relationshipDetails) {
    this.reviewSlider.emit({personEntityId: relationshipDetails.entityId, personEntityHeader :relationshipDetails.entityName});
  }

  getMessage() { 
    if (this.getValuesFormCOIEntityObj('versionStatus') == 'ARCHIVE')
    return 'Entity modified';
    else if ((this.reqObject.isRelationshipActive && !this.getValuesFormCOIEntityObj('isActive')))
    return 'Entity inactivated';
  }

  checkForEntityWarning() {
    return this.getValuesFormCOIEntityObj('versionStatus') == 'ARCHIVE' || (this.reqObject.isRelationshipActive && !this.getValuesFormCOIEntityObj('isActive'));
  }

  getHelpText() {
    if (this.referredFrom != 'SFI_SUMMARY' && this.getValuesFormCOIEntityObj('versionStatus') == 'ARCHIVE')
    return 'Please click Modify button to revise SFI';
    else if (this.referredFrom != 'SFI_SUMMARY' && (this.reqObject.isRelationshipActive && !this.getValuesFormCOIEntityObj('isActive')))
    return 'Please use Inactivate button to inactivate SFI';
  }
  
  groupBy(jsonData, key, innerKey) {
    return jsonData.reduce((relationsTypeGroup, item) => {
        (relationsTypeGroup[item[key][innerKey]] = relationsTypeGroup[item[key][innerKey]] || []).push(item);
        return relationsTypeGroup;
    }, {});
  }
}
