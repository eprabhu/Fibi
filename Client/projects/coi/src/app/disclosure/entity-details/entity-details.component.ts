import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { EntityDetailsService } from './entity-details.service';

@Component({
  selector: 'app-entity-details',
  templateUrl: './entity-details.component.html',
  styleUrls: ['./entity-details.component.scss']
})
export class EntityDetailsComponent implements OnInit,OnDestroy {

  constructor(public entityDetailService:EntityDetailsService,private _route:ActivatedRoute) {
    this.clearSfiNavBarStyle();
  }
  entityDetails = {};
  ngOnInit() {
    const isEditMode = this._route.snapshot.queryParamMap.get('mode') === 'edit'
    if(isEditMode){
      this.entityDetailService.isExpanded = false;
    }
  }

  ngOnDestroy(): void {
    this.entityDetailService.isExpanded = true;
  }
   clearSfiNavBarStyle() {
    document.body.style.removeProperty('overflow');
  }
}
