/*  
 *  Description: Class to store properties about student demonstrators
 *  inheriting the Staff class.
*/
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Coursework
{
    public class Demonstrator : Staff
    {
        public Demonstrator() { }

        //  String storing the course the Demonstrator is on
        private string course;
        public string Course
        {
            get
            {
                return course;
            }
            set
            {
                course = checkString(value);
            }
        }

        //  String storing the Demonstrator's supervisor
        private string supervisor;
        public string Supervisor
        {
            get
            {
                return supervisor;
            }
            set
            {
                supervisor = checkString(value);
            }
        }

        //  Method to override Staff calcTaxRate to return 5
        public override float calcTaxRate(int grossPayPence)
        {
            return 5;
        }
    }
}
