import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { SharedSfiService } from './shared-sfi.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { SfiObject } from '../shared-interface';

@Component({
  selector: 'app-shared-sfi-card',
  templateUrl: './shared-sfi-card.component.html',
  styleUrls: ['./shared-sfi-card.component.scss'],
  providers: [SharedSfiService]
})

export class SharedSfiCardComponent implements OnInit, OnDestroy {

  @Input() reqObject: any;
  @Input() referredFrom: 'SFI_SUMMARY' | 'SFI_EDIT_AND_DASHBOARD' | 'TRAVEL_DISCLOSURE';
  @Output() viewSlider = new EventEmitter<any>();
  @Output() deleteEvent =  new EventEmitter<any>();
  @Output() activateDeactivateEvent =  new EventEmitter<any>();

  SFIObject = new SfiObject();
  $subscriptions: Subscription[] = [];

  constructor(private _router: Router, private _sharedSFIService: SharedSfiService, private _commonService: CommonService) { }

    ngOnInit() {
      this.updateSFIObject();
    }

    ngOnDestroy() {
      subscriptionHandler(this.$subscriptions);
    }

  private updateSFIObject(): void {
    if (this.reqObject) {
      this.SFIObject.isActive = this.referredFrom =='SFI_EDIT_AND_DASHBOARD' ? this.setActiveInEditMode() : this.setActiveInViewMode();
      this.SFIObject.entityId =  this.reqObject.personEntityId;
      this.SFIObject.entityType = this.getEntityDescription();
      this.SFIObject.countryName = this.getCountryName();
      this.SFIObject.involvementEndDate = this.reqObject.involvementEndDate;
      this.SFIObject.involvementStartDate = this.reqObject.involvementStartDate;
      this.SFIObject.validPersonEntityRelTypes = this.reqObject.validPersonEntityRelTypes;
      this.SFIObject.entityName = this.getValuesFormCOIEntityObj('entityName');
    }
  }

  setActiveInEditMode() {
    return   this.reqObject.versionStatus === 'PENDING' ? 'DRAFT' :
              this.reqObject.versionStatus === 'ACTIVE' && this.reqObject.isRelationshipActive ? 'ACTIVE' :
              this.reqObject.versionStatus === 'ACTIVE' && !this.reqObject.isRelationshipActive ? 'INACTIVE' : '';
  }

  setActiveInViewMode() {
    return  this.reqObject.versionStatus === 'PENDING' ? 'DRAFT' :
    (this.reqObject.versionStatus === 'ACTIVE' || this.reqObject.versionStatus === 'ARCHIVE') && this.reqObject.isRelationshipActive ? 'ACTIVE' :
    this.reqObject.versionStatus === 'ACTIVE' && !this.reqObject.isRelationshipActive ? 'INACTIVE' : '';
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
    this.viewSlider.emit({flag: true, entityId: entityId});
  }

    modifySfiDetails(entityId: number, mode: string): void {
        this.$subscriptions.push(this._sharedSFIService.modifySfi({ personEntityId: entityId }).subscribe((res: any) => {
            this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: res.personEntityId, mode: mode } });
        }));
    }

  deleteConfirmation() {
    this.deleteEvent.emit({eId: this.SFIObject.entityId});
  }

  activateDeactivate() {
    this.activateDeactivateEvent.emit(this.reqObject);
  }

}
