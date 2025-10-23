using Xunit;

public class T1Tests
{
    [Fact]
    public void Test_Sum_Of_Two_Numbers()
    {
        int a = 2;
        int b = 3;
        int result = a + b;
        Assert.Equal(5, result);
    }

    [Fact]
    public void Test_String_Is_Not_Null()
    {
        string str = "hello";
        Assert.NotNull(str);
    }

    [Fact]
    public void Test_List_Contains_Element()
    {
        var list = new System.Collections.Generic.List<int> { 1, 2, 3 };
        Assert.Contains(2, list);
    }
}