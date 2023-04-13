import { Component, Input, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityDetailsService } from '../entity-details.service';
import { SfiService } from '../../sfi/sfi.service';

@Component({
  selector: 'app-add-entity-details',
  templateUrl: './add-entity-details.component.html',
  styleUrls: ['./add-entity-details.component.scss']
})
export class AddEntityDetailsComponent implements OnInit {

  isExpanded = true;
  entityDetails:any = {};
  constructor(private _entityDetailsServices:EntityDetailsService,private _router: Router,
    public sfiServices:SfiService) { }

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
