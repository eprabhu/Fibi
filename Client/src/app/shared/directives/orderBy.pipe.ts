import {Pipe, PipeTransform} from '@angular/core';

@Pipe({name: 'orderBy'})
export class OrderByPipe implements PipeTransform {

    transform(records: Array<any>, args?: any): any {

        const getKeyValueFromObject: any | null = (object: any, keyNames: string[]) => {
            let obj = object;
            for (let i = 0; i < keyNames.length; i++) {
                if (!obj) {
                    return null;
                }
                obj = obj[keyNames[i]];
            }
            return obj;
        };

        return records.sort(function (a, b): number {

            let valueA = getKeyValueFromObject(a, args.property.split('.'));
            valueA = typeof valueA === 'string' ? valueA.toLowerCase() : valueA;
            let valueB = getKeyValueFromObject(b, args.property.split('.'));
            valueB = typeof valueB === 'string' ? valueB.toLowerCase() : valueB;

            if (valueA < valueB) {
                return -1 * args.direction;
            } else if (valueA > valueB) {
                return 1 * args.direction;
            } else {
                return 0;
            }
        });
    }


}
