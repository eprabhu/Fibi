import { Component, OnInit } from '@angular/core';
import { EntityDetailsService } from '../../entity-details.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-view-sfi-details',
  templateUrl: './view-sfi-details.component.html',
  styleUrls: ['./view-sfi-details.component.scss']
})
export class ViewSfiDetailsComponent implements OnInit {

  isExpanded = true;
  entityDetails:any = {};
  constructor(public _entityDetailsServices:EntityDetailsService,private _router: Router) { }

  ngOnInit() {
    this.getEntityDetails();
    // this.sfiServices.isShowSfiNavBar
  }

  getEntityDetails(){
    this._entityDetailsServices.$entityDetails.subscribe((res:any)=>{
      this.entityDetails = res.coiFinancialEntity;
    })
  }

  navigateBack() {
    this._router.navigate(['/coi/user-dashboard/entities'])
    // this._router.navigateByUrl(this._entityDetailsServices.previousURL);
  }

}
